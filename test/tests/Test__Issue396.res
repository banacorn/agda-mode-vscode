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
//   AGDA_TEST_GLOB="Test__Issue396*.js" npm test

// Holds the first `Editor.Text.batchReplace` until released, and counts every
// call. Goals and Editor are imported by the tests from lib/js, while the
// running extension uses its webpack bundle, so this patches the same compiled
// Editor module that `Goals.scanAllGoals` calls here.
module BatchReplaceGate = {
  type t

  let install: unit => t = %raw(`function() {
    const Editor = require("../../src/Editor.bs.js");
    const original = Editor.$$Text.batchReplace;
    let resolveEntered;
    let resolveRelease;
    const entered = new Promise(resolve => { resolveEntered = resolve; });
    const released = new Promise(resolve => { resolveRelease = resolve; });
    let calls = 0;

    Editor.$$Text.batchReplace = async function(document, replacements) {
      calls += 1;
      if (calls === 1) {
        resolveEntered();
        await released;
      }
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

describe("issue #396: busy scanAllGoals queues a rescan", () => {
  This.timeout(10000)

  let filename = "Issue396.agda"
  let fileContent = ref("")

  Async.before(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  let catchExn = async promise =>
    try {
      await promise
      Ok()
    } catch {
    | exn => Error(exn)
    }

  Async.it("a scan that finds the goals busy returns without recursing", async () => {
    let editor = await File.open_(Path.asset(filename))
    let document = VSCode.TextEditor.document(editor)
    let questionMark = String.indexOf(Editor.Text.getAll(document), "?")

    let goals = Goals.make()
    goals->Goals.addGoalPositions([(questionMark, questionMark + 1)])

    // Feed document changes to this Goals instance, as `Main` does for the
    // extension's own (src/Main/Main.res, `onDocumentChange`).
    let forwardedScans = []
    let stopForwarding = VSCode.Workspace.onDidChangeTextDocument(event => {
      if event->VSCode.TextDocumentChangeEvent.document == document {
        let changes =
          event
          ->VSCode.TextDocumentChangeEvent.contentChanges
          ->Array.map(TokenChange.fromTextDocumentContentChangeEvent)
          ->Array.toReversed
        if Array.length(changes) != 0 {
          forwardedScans->Array.push(catchExn(goals->Goals.scanAllGoals(editor, changes)))
        }
      }
    })

    let gate = BatchReplaceGate.install()
    // scan A: widens the `?` and holds the busy state at `batchReplace`
    let owner = catchExn(goals->Goals.resetGoalIndicesOnLoad(editor, [0]))
    await gate->BatchReplaceGate.entered

    // scan B: must only queue a rescan for A and return
    let contenderResult = await catchExn(goals->Goals.scanAllGoals(editor, []))

    gate->BatchReplaceGate.release
    let ownerResult = await owner
    let forwardedResults = await Promise.all(forwardedScans)
    gate->BatchReplaceGate.restore
    stopForwarding->VSCode.Disposable.dispose->ignore
    let serialized = goals->Goals.serializeGoals
    goals->Goals.destroy

    switch contenderResult {
    | Ok() => ()
    | Error(exn) => raise(exn)
    }
    switch ownerResult {
    | Ok() => ()
    | Error(exn) => raise(exn)
    }
    forwardedResults->Array.forEach(result =>
      switch result {
      | Ok() => ()
      | Error(exn) => raise(exn)
      }
    )
    // A's deferred rescan sees its completed rewrite, so only A's batch reaches
    // `Editor.Text.batchReplace`
    Assert.deepStrictEqual(gate->BatchReplaceGate.calls, 1)
    Assert.deepStrictEqual(
      Editor.Text.getAll(document),
      fileContent.contents->String.replace("?", "{!   !}"),
    )
    Assert.deepStrictEqual(serialized, ["#0 [6:5-12)"])
  })
})
