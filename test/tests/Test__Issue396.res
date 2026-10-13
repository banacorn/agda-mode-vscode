open Mocha
open Test__Util

// #396: `Goals.scanAllGoals` overflows the stack when a busy scan queues a rescan.
//
// Scan A widens a `?` and waits at `Editor.Text.batchReplace` while holding the
// busy state. Scan B finds the goals busy and sets `rescanPending`, but then
// consumes that flag itself and calls `scanAllGoals` again. The new call takes
// the same busy path without awaiting anything, so the calls nest until
// JavaScript raises `RangeError: Maximum call stack size exceeded`.
//
// Relatedly, a scan with no rewrites calls `setNotBusy` although it never set
// busy, clearing the busy state of a scan that is still rewriting.
//
//   AGDA_TEST_GLOB="Test__Issue396*.js" npm test

// Holds the first `Editor.Text.batchReplace` until released, and counts every
// call. With `holdAfterApply`, the edit is applied first and the call is held
// before it returns; otherwise it is held before the edit is applied.
// Goals and Editor are imported by the tests from lib/js, while the running
// extension uses its webpack bundle, so this patches the same compiled Editor
// module that `Goals.scanAllGoals` calls here.
module BatchReplaceGate = {
  type t

  let install: (~holdAfterApply: bool) => t = %raw(`function(holdAfterApply) {
    const Editor = require("../../src/Editor.bs.js");
    const original = Editor.$$Text.batchReplace;
    let resolveEntered;
    let resolveRelease;
    const entered = new Promise(resolve => { resolveEntered = resolve; });
    const released = new Promise(resolve => { resolveRelease = resolve; });
    let calls = 0;

    Editor.$$Text.batchReplace = async function(document, replacements) {
      calls += 1;
      if (calls !== 1) {
        return original(document, replacements);
      }
      if (holdAfterApply) {
        const applied = await original(document, replacements);
        resolveEntered();
        await released;
        return applied;
      }
      resolveEntered();
      await released;
      return original(document, replacements);
    };

    return {
      entered,
      release: resolveRelease,
      restore: function() { Editor.$$Text.batchReplace = original; },
      calls: function() { return calls; },
    };
  }`)

  @get external entered: t => promise<unit> = "entered"
  @send external release: t => unit = "release"
  @send external restore: t => unit = "restore"
  @send external calls: t => int = "calls"
}

let catchExn = async promise =>
  try {
    await promise
    Ok()
  } catch {
  | exn => Error(exn)
  }

let raiseIfError = result =>
  switch result {
  | Ok() => ()
  | Error(exn) => raise(exn)
  }

// Feeds document changes to a Goals instance, as `Main` does for the
// extension's own (src/Main/Main.res, `onDocumentChange`).
module ForwardChanges = {
  type t = {
    scans: array<promise<result<unit, exn>>>,
    firstScan: promise<unit>,
    disposable: VSCode.Disposable.t,
  }

  let make = (goals, editor) => {
    let document = VSCode.TextEditor.document(editor)
    let scans = []
    let (firstScan, resolveFirstScan, _) = Util.Promise_.pending()
    let disposable = VSCode.Workspace.onDidChangeTextDocument(event => {
      if event->VSCode.TextDocumentChangeEvent.document == document {
        let changes =
          event
          ->VSCode.TextDocumentChangeEvent.contentChanges
          ->Array.map(TokenChange.fromTextDocumentContentChangeEvent)
          ->Array.toReversed
        if Array.length(changes) != 0 {
          let scan = catchExn(goals->Goals.scanAllGoals(editor, changes))
          scans->Array.push(scan)
          resolveFirstScan()
        }
      }
    })
    {scans, firstScan, disposable}
  }

  let dispose = self => self.disposable->VSCode.Disposable.dispose->ignore
}

describe("issue #396: busy scanAllGoals queues a rescan", () => {
  This.timeout(10000)

  let filename = "Issue396.agda"
  let fileContent = ref("")

  Async.before(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  // a Goals instance with one goal at the fixture's `?`
  let setup = async () => {
    let editor = await File.open_(Path.asset(filename))
    let document = VSCode.TextEditor.document(editor)
    let questionMark = String.indexOf(Editor.Text.getAll(document), "?")
    let goals = Goals.make()
    goals->Goals.addGoalPositions([(questionMark, questionMark + 1)])
    (editor, document, goals)
  }

  Async.it("a scan that finds the goals busy returns without recursing", async () => {
    let (editor, document, goals) = await setup()
    let forward = ForwardChanges.make(goals, editor)

    let gate = BatchReplaceGate.install(~holdAfterApply=false)
    // scan A: widens the `?` and holds the busy state at `batchReplace`
    let owner = catchExn(goals->Goals.resetGoalIndicesOnLoad(editor, [0]))
    await gate->BatchReplaceGate.entered

    // scan B: must only queue a rescan for A and return
    let contenderResult = await catchExn(goals->Goals.scanAllGoals(editor, []))

    gate->BatchReplaceGate.release
    let ownerResult = await owner
    let forwardedResults = await Promise.all(forward.scans)
    gate->BatchReplaceGate.restore
    forward->ForwardChanges.dispose
    let serialized = goals->Goals.serializeGoals
    goals->Goals.destroy

    raiseIfError(contenderResult)
    raiseIfError(ownerResult)
    forwardedResults->Array.forEach(raiseIfError)
    // A's deferred rescan sees its completed rewrite, so only A's batch reaches
    // `Editor.Text.batchReplace`
    Assert.deepStrictEqual(gate->BatchReplaceGate.calls, 1)
    Assert.deepStrictEqual(
      Editor.Text.getAll(document),
      fileContent.contents->String.replace("?", "{!   !}"),
    )
    Assert.deepStrictEqual(serialized, ["#0 [6:5-12)"])
  })

  Async.it("a scan with no rewrites leaves another scan's busy state alone", async () => {
    let (editor, _, goals) = await setup()
    let forward = ForwardChanges.make(goals, editor)

    let gate = BatchReplaceGate.install(~holdAfterApply=true)
    // scan A: widens the `?`, then holds the busy state before
    // `batchReplace` returns
    let owner = catchExn(goals->Goals.resetGoalIndicesOnLoad(editor, [0]))
    await gate->BatchReplaceGate.entered

    // the scan of A's own edit: the goal is now an intact hole, so it has
    // nothing to rewrite and must not touch the busy state that A holds
    await forward.firstScan
    let earlyForwardedResults = await Promise.all(forward.scans)
    let busyWhileOwnerHeld = goals->Goals.isBusy

    gate->BatchReplaceGate.release
    let ownerResult = await owner
    let forwardedResults = await Promise.all(forward.scans)
    gate->BatchReplaceGate.restore
    forward->ForwardChanges.dispose
    goals->Goals.destroy

    earlyForwardedResults->Array.forEach(raiseIfError)
    raiseIfError(ownerResult)
    forwardedResults->Array.forEach(raiseIfError)
    Assert.deepStrictEqual(busyWhileOwnerHeld, true)
  })
})
