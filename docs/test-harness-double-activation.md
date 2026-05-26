# Test-harness double-activation bug

A test-infrastructure bug in `agda-mode-vscode`. **Independent of issue #264.** This document captures the bug, its mechanism, the fix, and the three test-side collateral changes required after fixing it.

If you are reapplying this on `master` (or any branch where the fix isn't landed), the four source edits below are sufficient to fix the duplication itself. The collateral test changes are needed to keep `npm test` green afterwards.

---

## 1. The bug

When `npm test` runs, the VS Code extension is **activated twice** in the same Node/Electron process:

- **Activation A** — VS Code's normal dev-extension load: VS Code reads `package.json#main` (`./dist/app.bundle.js`) from `extensionDevelopmentPath`, requires the webpack bundle, and calls its `activate(context)`. That bundle has its own compiled-in copy of `src/Main/Main.res`, so it runs `Main.activateWithoutContext` → builds its own `State.t` with its own `state.channels` (each `Chan.make()` constructs a fresh `EventEmitter3`).
- **Activation B** — the test code calls it again. `test/tests/Test__Util.res:activateExtension` explicitly invokes `Main.activateWithoutContext` from the **`lib/js`** compilation of the same source. That's a separate module instance in Node's module cache (the resolved path is different: `lib/js/src/Main/Main.bs.js`, not `dist/app.bundle.js`). It builds a *second* `State.t` with a *second* set of channels.

Because Node caches modules by resolved path, the two compiled copies share **no module-level state**. `Chan.make()` from A and `Chan.make()` from B produce different `EventEmitter3` objects. `Registry__Connection`'s singleton `status`/`makeCount` refs (`src/Registry__Connection.res:72-75`) exist twice. Two separate `commandHandled` channels, two separate registries, two separate States.

VS Code's command registry only accepts one callback per command name. The second `registerCommand("agda-mode.load", ...)` from activation B throws `Error: command 'agda-mode.load' already exists`. So when a test calls `executeCommand("agda-mode.load")`, the callback that actually runs is the one from activation A (webpack bundle, whichever registered first). That callback emits its completion on **A's** `commandHandled`. The test harness subscribed on **B's** `commandHandled`. The subscriber never fires.

Symptoms a developer can see on `master`:

- `Error: command 'agda-mode.load' already exists` (or silent if a `try/catch` is swallowing it).
- Any test in the suite that relies on `state.channels.commandHandled` / `state.channels.log` events from a `Chan` subscribed on the test side either races, observes nothing, or silently sees a half-empty stream.
- Tests that *do* "work" on master pass despite the double activation, but the exact mechanism by which the "command 'agda-mode.load' already exists" error is tolerated has not been verified. Don't assume the suite-pass count is owed to design — it's incidental. Under `Async.it.only` isolation in particular, the timing flips and tests hang at the first `executeCommand` round-trip with the duplicate-registration error surfacing explicitly.

---

## 2. Repro on master

You don't need the issue #264 regression test or any timer-removal work to *see* the duplication. Add a single sentinel:

```rescript
// src/Main/Main.res, inside `activateWithoutContext`, near the top
Js.log("[REPRO] Main.activateWithoutContext invoked")
```

Then:

```bash
npm run build
npm test > /tmp/repro.log 2>&1
grep -c '\[REPRO\] Main.activateWithoutContext invoked' /tmp/repro.log
```

Expected: **at least 2** matches per test process. Each match is one activation. Removing the sentinel after observation is the whole repro.

If you want to *also* see the mismatch at the channel level, an even stronger probe tags each `Chan` module instance with a random fingerprint plus the resolved `__filename` and logs both from inside `Chan.on`/`Chan.emit`. The expected output is two distinct fingerprints, one pointing at `lib/js/src/Util/Chan.bs.js` and one at `dist/app.bundle.js`. Skip this unless the basic repro doesn't convince you. (If you have access to the `issue#264` branch, the same probe is documented in detail in `docs/issue-264-findings.md` as probe #17.)

---

## 3. The fix (4 files)

The strategy: stop calling `Main.activateWithoutContext` from the test code. Instead, fetch the **live** extension's State via VS Code's extension API. To make that possible, change `Main.activate`'s return value so the test harness can reach both the `channels` and the `memento` it needs.

### 3.1 `src/Main/Main.res`

Add a record type for the activation exports:

```rescript
type activationExports = {
  channels: State.channels,
  memento: Memento.t,
}
```

(Insert it immediately before `let activateWithoutContext = (...)` — currently around line 273.)

Change `let activate` (currently around line 429) so it constructs the workspace-backed memento explicitly and returns the new shape:

```rescript
let activate = (platformDeps, context): activationExports => {
  let subscriptions = VSCode.ExtensionContext.subscriptions(context)
  let extensionUri = VSCode.ExtensionContext.extensionUri(context)
  let globalStorageUri = VSCode.ExtensionContext.globalStorageUri(context)
  let memento = Memento.make(Some(VSCode.ExtensionContext.workspaceState(context)))

  let channels = activateWithoutContext(
    platformDeps,
    subscriptions,
    extensionUri,
    globalStorageUri,
    memento,
  )

  {channels, memento}
}
```

The signature of `activateWithoutContext` does **not** change. Production behavior is also unchanged — `exports.memento` is identical to the memento the rest of the extension uses internally; only test code reads it.

### 3.2 `src/Main/Desktop.res`

Annotate the entry point's return type so a future drift in `Main.activate` won't be silently absorbed:

```rescript
let activate = (context): Main.activationExports => Main.activate(make(), context)
```

(Replaces the existing `let activate = context => { Main.activate(make(), context) }` around line 84.)

### 3.3 `src/Main/Web.res`

Same as Desktop, around line 44:

```rescript
let activate = (context): Main.activationExports => Main.activate(make(), context)
```

### 3.4 `test/tests/Test__Util.res`

Replace the body of `activateExtension` (currently around lines 162–184). The version on master calls `Main.activateWithoutContext` directly with a stub `Memento.make(None)`; replace it with this:

```rescript
let activateExtension = async (candidate): State.channels => {
  switch activationSingleton.contents {
  | Some(channels) => channels
  | None =>
    switch VSCode.Extensions.getExtension("banacorn.agda-mode") {
    | None => raise(Failure("Extension banacorn.agda-mode not found"))
    | Some(extension) =>
      let exports: Main.activationExports = await extension->VSCode.Extension.activate
      await exports.memento->Memento.PreferredCandidate.set(candidate)
      activationSingleton := Some(exports.channels)
      exports.channels
    }
  }
}
```

Two important details:

- `VSCode.Extensions.getExtension` / `VSCode.Extension.activate` are pre-existing bindings in `node_modules/rescript-vscode/src/VSCode.res`. No new bindings required.
- The `candidate` parameter is **not** vestigial. It must be written to the **live** extension's workspace-backed memento (the one constructed in step 3.1 from `VSCode.ExtensionContext.workspaceState(context)`). The previous code wrote it to a *stub* `Memento.make(None)` that the webpack-bundle State never read; tests passed only because the lib/js activation happened to win the registration race and serve commands with that stub memento. Dropping this line breaks every test that needs an Agda binary path (~150 failures with `No Agda version found`).

### 3.5 Build

```bash
npm run build
```

`npx rescript` alone is insufficient — VS Code loads the webpack bundle, so the bundle itself must be rebuilt for the changed `Main.activate` shape to take effect.

---

## 4. Collateral test regressions

Even with all four edits above, three tests in the suite go from passing to failing. They were observing internal state that only existed under the dual-activation duplication; after the fix, the lib/js side no longer participates in command serving and those observations become impossible.

| Test | Why it regresses | Fix shape |
|---|---|---|
| `Connection / State__Connection.sendRequest should log ActivationFlow for reused existing connection` | Called `State__Connection.sendRequestAndCollectResponses(Request.Load)` directly on `ctx.state`. That skips the command-level flow where `ActivationFlow` events are emitted; under dual-State some other side-effect was producing them. | Route through the command path. |
| `Registry__Connection / should share a single connection mechanism between two different Agda files` | Called `Registry__Connection.inspect()` to assert `makeCount == 1` etc. Reads the *lib/js* singleton refs, which are no longer touched after the fix. | Weaker, log-based assertion. |
| `Token Bookkeeping / should remove Tokens after Command.Quit` | `AgdaMode.quit` called `Registry.removeAndDestroy` on the orphaned lib/js registry. The live State was never actually destroyed, so its `tokens` were never cleared. | Route `quit` through the live command. |

### 4.1 `Connection / ActivationFlow for reused existing connection`

In `test/tests/Test__Connection.res`, one-line change in the body of the test `"State__Connection.sendRequest should log ActivationFlow for reused existing connection"`:

```diff
-        let _ = await ctx.state->State__Connection.sendRequestAndCollectResponses(Request.Load)
+        await ctx->AgdaMode.execute(Load)
```

Note: the test title now somewhat misrepresents what it exercises (it exercises the command path, not `State__Connection.sendRequest` directly). Either rename the test or accept the label drift; both are acceptable.

### 4.2 `Registry__Connection / single connection mechanism between two Agda files`

In `test/tests/Test__Registry__Connection.res`, the original test asserts four invariants via `Registry__Connection.inspect()`: `makeCount == 1`, `userCount == 2` after second load, `userCount == 1` after first quit, `status == "Empty"` after second quit. None of those are observable post-fix without exposing a registry inspector through `Main.activationExports`.

Replace those assertions with a log-based check on the second `makeAndLoad`'s ActivationFlow events:

```rescript
// Load first file
let agdaA = await AgdaMode.makeAndLoad("GotoDefinition.agda")

let listener = Log.collect(agdaA.channels.log)

// Load second file
let agdaB = await AgdaMode.makeAndLoad("Lib.agda")

let activationEvents =
  listener()
  ->Array.filterMap(log =>
    switch log {
    | Log.Connection(Log.Connection.ActivationFlow(event)) =>
      Some(switch event {
      | Log.Connection.ActivationFlow.ActivationStarted => "ActivationStarted"
      | Log.Connection.ActivationFlow.ExistingConnectionReused => "ExistingConnectionReused"
      | Log.Connection.ActivationFlow.FreshEstablishStarted => "FreshEstablishStarted"
      | Log.Connection.ActivationFlow.ActivationSucceeded => "ActivationSucceeded"
      | Log.Connection.ActivationFlow.ActivationFailed => "ActivationFailed"
      })
    | _ => None
    }
  )

Assert.deepStrictEqual(activationEvents, [
  "ActivationStarted",
  "ExistingConnectionReused",
  "ActivationSucceeded",
])

await AgdaMode.quit(agdaA)
await AgdaMode.quit(agdaB)
```

**Acknowledge the cost:** this is genuinely weaker than the original. The old test verified that exactly one OS process was spawned and that user-counting was correct on each quit. The new test only verifies that the second `makeAndLoad` emits an `ExistingConnectionReused` event — it does not verify the count, nor the userCount transitions, nor the post-quit empty status. The proper recovery (future work) is to add a `registryInspect: unit => Registry__Connection.view` field to `activationExports` so the test can call the live inspector. Until then, the log-based version is the strongest stable test possible without a wider source surface.

### 4.3 `Token Bookkeeping / should remove Tokens after Command.Quit`

The test itself does not change. The fix is in the shared harness helper `AgdaMode.quit` in `test/tests/Test__Util.res`. Replace the body (around the `let quit = async (self: t) => ...` line — currently a one-liner that calls `Registry.removeAndDestroy`):

```rescript
let quit = async (self: t) => {
  let _ = await File.open_(self.filepath)
  let destroyCompleted = Log.on(
    self.channels.log,
    log =>
      switch log {
      | Log.Others("State.destroy: Connection released, destruction complete") => true
      | _ => false
      },
  )

  switch await executeCommand("agda-mode.quit") {
  | None => ()
  | Some(Ok(_)) => ()
  | Some(Error(error)) =>
    let (header, body) = Connection.Error.toString(error)
    raise(Failure(header ++ "\n" ++ body))
  }

  await destroyCompleted
}
```

The log barrier (`"State.destroy: Connection released, destruction complete"`) is emitted at `src/State/State.res:135`. Verify it's there on the target branch before applying this — if it isn't, the helper will hang at `await destroyCompleted` to the 10s mocha timeout.

This is a **suite-wide harness change.** It affects every test that calls `ctx->AgdaMode.quit`. As of this writing the user list is `Test__Refine`, `Test__SolveConstraints`, `Test__SearchAbout`, `Test__InferType`, `Test__ElaborateAndGive`, `Test__GoalTypeAndContext`, `Test__GoalTypeContextAndInferredType`, `Test__Context`, `Test__TokenBookkeeping`. Before applying, re-grep your test directory:

```bash
grep -rn "AgdaMode\.quit\|->AgdaMode\.quit" test/ --include='*.res'
```

so you know the actual current callers and can prioritize the full-suite run accordingly.

---

## 5. Verification

```bash
npm run build
npm test > /tmp/test.log 2>&1
grep -nE "passing|failing|pending" /tmp/test.log | tail -5
grep -c "already exists" /tmp/test.log
grep -c "No Agda version found" /tmp/test.log
```

Expected after all edits:

- `passing|failing|pending` counts match your **pre-fix baseline**. The total `passing` count is branch-specific and may differ from any reference number; the meaningful check is "no new failures introduced." If your pre-fix baseline already had failing tests (e.g. `#264`'s minimal reproducer on the issue#264 branch), they remain — this doc is not a fix for any product bug, only for the test-harness double activation.
- `already exists` count: **0**.
- `No Agda version found` count: **0**.

**Important capture note:** use plain shell redirect (`> /tmp/test.log 2>&1`). Do **not** wrap with `script -q`, `unbuffer`, `expect`, or any PTY-injection tool. In this repo, that reliably SIGABRTs the VS Code test host before any tests run.

If you see hundreds of `No Agda version found` lines, you forgot the `Memento.PreferredCandidate.set` call in step 3.4.

If you see `already exists` lines, the `Main.activate` change didn't take or the build didn't rebuild the webpack bundle.

If `Test__Refine On GiveString 2` (or similar token-/connection-lifecycle tests) regress, the suite-wide `quit` change in step 4.3 wasn't applied or the destruction-complete log marker is missing on the target branch.

---

## 6. Why this works

After the four source edits:

- The webpack bundle's `Main.activate` constructs the workspace-backed memento and exposes `{channels, memento}` as the extension's `exports`.
- VS Code caches that exports object and returns the same reference for every `vscode.extensions.getExtension("banacorn.agda-mode").exports` lookup.
- `test/tests/Test__Util.res:activateExtension` retrieves it via `Extension.activate` (idempotent — triggers activation if needed, returns cached exports otherwise) and writes `PreferredCandidate` into the same memento the live extension will read on the first `agda-mode.load`.
- The test harness no longer calls `Main.activateWithoutContext`. Only one `State.t` exists. The lib/js `Chan` module is loaded (because test code imports types from it), but no second `State.t` is constructed; `Registry__Connection`'s lib/js refs stay at their defaults and are never touched.
- `state.channels.commandHandled` subscribed in tests is the same `Chan.t` (same underlying `EventEmitter3`) that `sendAgdaRequest` emits on. Events flow.

The duplication of *module instances* still happens — Node will still load `Chan` once for the webpack bundle and once for the lib/js test compilation. But because the test code no longer **uses** the lib/js `Chan` to make any singleton state (no `State.t`, no command registration, no registry entries), the duplication has no observable consequence beyond a slightly larger heap.
