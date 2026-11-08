open Mocha
open Test__Util

// #264: a load used to save and replace the shared offset snapshot before
// waiting for the connection. Finishing the first load then erased the second
// load's edit history. Its answer registered the opening `{` of an expanded hole
// as a one-character `?`, and scanAllGoals kept expanding that character forever.
module EditLimit = {
  type t
  let install: unit => t = %raw(`function() {
    const vscode = require("vscode");
    const original = vscode.workspace.applyEdit;
    let count = 0;
    vscode.workspace.applyEdit = function(...args) {
      if (++count > 40) {
        throw new Error("#264: more than 40 edits while completing two loads");
      }
      return original.apply(this, args);
    };
    return { restore() { vscode.workspace.applyEdit = original; } };
  }`)
  @send external restore: t => unit = "restore"
}

// RequestSent means the second load has reached the connection queue. Hold the
// first answer until then, so the bad ordering is deterministic. The test does
// not require the second load to actually run before the first one finishes.
let withOverlappingReload = async (ctx: AgdaMode.t, ~afterMakeCase=false, ~outsideResponse=false, operation) => {
  let sawMakeCase = ref(!afterMakeCase)
  let raced = ref(false)
  let extraLoad = ref(None)
  let (queued, resolveQueued, _) = Util.Promise_.pending()
  let (start, resolveStart, _) = Util.Promise_.pending()
  // A user command does not inherit a response's reentrant connection owner.
  // Create this async chain outside the middleware to exercise that path too.
  let outsideLoad = if outsideResponse {
    let run = async () => {
      await start
      await executeCommand("agda-mode.load")
    }
    Some(run())
  } else {
    None
  }
  let disposeLog = ctx.channels.log->Chan.on(log => {
    switch log {
    | RequestSent(Load) if raced.contents => resolveQueued()
    | _ => ()
    }
  })
  ctx.state.middlewares
  ->Array.push(handler => async response => {
    switch response {
    | Response.MakeCase(_, _) => sawMakeCase := true
    | _ => ()
    }
    switch response {
    | Response.InteractionPoints(_) if sawMakeCase.contents && !raced.contents =>
      raced := true
      extraLoad := switch outsideLoad {
      | None => Some(executeCommand("agda-mode.load"))
      | Some(load) =>
        resolveStart()
        Some(load)
      }
      await queued
    | _ => ()
    }
    await handler(response)
  })
  ->ignore
  // Stop a broken build before it edits indefinitely. No edit is altered below
  // that limit, and the assertions check the whole document and goal ranges.
  let guard = EditLimit.install()
  let result = try {
    await operation()
    switch extraLoad.contents {
    | None => Assert.fail("the overlapping reload was never started")
    | Some(load) =>
      switch await load {
      | Some(Ok(_)) => ()
      | _ => Assert.fail("the overlapping reload failed")
      }
    }
    await Goals.waitUntilNotBusy(ctx.state.goals)
    Ok()
  } catch {
  | exn => Error(exn)
  }
  guard->EditLimit.restore
  disposeLog()
  ctx.state.middlewares->Array.pop->ignore
  switch result {
  | Ok() => ()
  | Error(exn) => raise(exn)
  }
}

let split = async (ctx: AgdaMode.t, payload) => {
  let goal = Goals.getGoalByIndex(ctx.state.goals, 0)->Option.getExn
  await AgdaMode.case(
    ctx,
    ~cursor=VSCode.TextDocument.positionAt(ctx.state.document, goal.start + 2),
    ~payload,
  )
}

// Submit the empty variable prompt, which introduces the function argument.
let introduceArgument = async (ctx: AgdaMode.t) => {
  let editor = await File.open_(ctx.filepath)
  let goal = Goals.getGoalByIndex(ctx.state.goals, 0)->Option.getExn
  Editor.Cursor.set(editor, VSCode.TextDocument.positionAt(ctx.state.document, goal.start + 2))
  let (loaded, resolveLoaded, _) = Util.Promise_.pending()
  let dispose = ctx.channels.commandHandled->Chan.on(command => {
    if command == Load {
      resolveLoaded()
    }
  })
  let command = executeCommand("agda-mode.case")
  let rec waitForPrompt = async () => {
    if ctx.state.panelCache.prompt->Option.isNone {
      await wait(5)
      await waitForPrompt()
    }
  }
  await waitForPrompt()
  switch ctx.state.panelCache.prompt {
  | Some((_, _, callback)) => await callback(PromptSuccess(""))
  | None => Assert.fail("the case-split prompt was never opened")
  }
  await loaded
  let _ = await command
  dispose()
}

describe("issue #264: overlapping loads", () => {
  This.timeout(20000)
  let fixtures = ["Issue264Overlap.agda", "Issue264PlainReloads.agda"]
  let original =
    fixtures->Array.map(filename =>
      NodeJs.Fs.readFileSync(Path.asset(filename))->NodeJs.Buffer.toString
    )
  let withLF = text => text->String.replaceAll("\r\n", "\n")
  Async.afterEach(async () => {
    await Registry.removeAndDestroyAll()
    for i in 0 to Array.length(fixtures) - 1 {
      await File.write(Path.asset(fixtures->Array.getUnsafe(i)), original->Array.getUnsafe(i))
    }
  })

  Async.it(
    "keeps the hole intact when reloading during the second dependent-record split",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("Issue264Overlap.agda")
      await introduceArgument(ctx)
      await split(ctx, "x")
      await withOverlappingReload(ctx, ~afterMakeCase=true, () => split(ctx, "x"))
      Assert.deepStrictEqual(
        Editor.Text.getAll(ctx.state.document)->withLF,
        original
        ->Array.getUnsafe(0)
        ->withLF
        ->String.replace("test = ?", "test (fst₁ , fst₂ , snd₁) = {!   !}"),
      )
      Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [6:29-36)"])
    },
  )

  Async.it(
    "keeps a newly typed goal intact across two ordinary reloads without a case split",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("Issue264PlainReloads.agda")
      let document = ctx.state.document
      // A new, unregistered question mark is the only prerequisite; a record
      // and a MakeCase response are not needed to trigger the snapshot collision.
      let appended = "\ng : Nat\ng = ?\n"
      let position = VSCode.TextDocument.positionAt(
        document,
        Editor.Text.getAll(document)->String.length,
      )
      let applied = await Editor.Text.insert(document, position, appended)
      Assert.deepStrictEqual(applied, true)
      await withOverlappingReload(ctx, ~outsideResponse=true, () => AgdaMode.execute(ctx, Load))
      Assert.deepStrictEqual(
        Editor.Text.getAll(document)->withLF,
        (original->Array.getUnsafe(1)->withLF ++ appended)->String.replaceAll("?", "{!   !}"),
      )
      Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [4:5-12)", "#1 [7:5-12)"])
    },
  )
  Async.it("lets the automatic reload finish when a user reload is queued behind Case", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue264Overlap.agda")
    await introduceArgument(ctx)
    await split(ctx, "x")
    // Queue the user load at Case's InteractionPoints, before MakeCase asks
    // for its own reentrant load. Taking the load lock before the connection
    // lock would deadlock: user load waits for Case, Case waits for user load.
    await withOverlappingReload(ctx, ~outsideResponse=true, () => split(ctx, "x"))
    Assert.deepStrictEqual(
      Editor.Text.getAll(ctx.state.document)->withLF,
      original->Array.getUnsafe(0)->withLF->String.replace("test = ?", "test (fst₁ , fst₂ , snd₁) = {!   !}"),
    )
    Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [6:29-36)"])
  })

})
