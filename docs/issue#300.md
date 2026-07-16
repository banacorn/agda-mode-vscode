# Investigation: #300 vs #318

## Summary

#300 is now reproducible automatically and deterministically.

The retained reproduction is `test/tests/Test__StaleEditorDecorationWarning.res`.
It runs as part of normal `npm test`, and can be isolated with:

```sh
AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test
```

The reproduction sequence is intentionally small:

1. activate the extension and open a real Agda file;
2. let the real Agda load populate token/decorations data;
3. switch tabs in the same editor group so the per-file listener closure keeps
   an old captured `TextEditor` while `state.editor` moves to the current one;
4. perform one type-then-delete text edit;
5. count the exact `TextEditor is closed/disposed` line in `exthost.log`.

The test currently asserts that the warning count increases. That makes it a
reproducer. The fix handoff below must convert the same test into a regression
test by keeping the same sequence and asserting that the warning count does not
increase after the product fix.

The candidate mechanism is still the same: `Main.initialize` registers global
workspace/window listeners per Agda file, and those listener closures use an
`editor` captured at initialization time without first checking that the event
belongs to that state's document. Once a tab switch makes that captured editor
stale, the real text-change listener can call decoration code with the stale
editor and make VS Code emit the disposed-editor warning.

#318 addressed a separate gradual-slowdown path in `Tokens.deltas`; it does not
address this stale-editor listener routing path. The current branch has kept
only reusable test infrastructure: `AGDA_TEST_GLOB` file selection,
`Test__Util.ExtHostLog`, and the behavior-named stale-editor warning
reproducer. The issue-specific probe, issue-specific harness, and env-based log
path plumbing are gone.

## Candidate mechanism behind #300: unfiltered global change listeners

Every Agda file that's opened triggers `initialize` (`src/Main/Main.res:44`),
which registers its own *global* listeners:

- `VSCode.Workspace.onDidChangeTextDocument` (`src/Main/Main.res:126`)
- `VSCode.Window.onDidChangeTextEditorSelection` (`src/Main/Main.res:114`)

These fire for **every** text-change/selection event in the workspace — not
just events belonging to that file's own editor/document — and the handlers
operate on the `editor` captured at `initialize`-time:

```rescript
VSCode.Workspace.onDidChangeTextDocument(event => {
  let changes = IM.Input.fromTextDocumentChangeEvent(editor, event)
  State__InputMethod.keyUpdateEditorIM(state, changes)->ignore
  state.tokens->Tokens.applyEdit(editor, event)   // always runs, regardless of which doc changed
  ...
})
```

There is no check that the event belongs to this state's document before doing
work, and there is no check that the editor used for editor-dependent work is
the current/live editor for that document. The retained reproducer exercises
the same-document form of the bug: the text-change event belongs to
`Goals.agda`, but the listener still passes the old initialization-time
captured editor after same-group tab switching has moved `state.editor` to a
fresh current editor for the same document. Calling VS Code APIs such as
`setDecorations` on that stale captured editor can trigger the
"TextEditor is closed/disposed" warning. H20 confirmed this causally in
isolation for `setDecorations` specifically (deterministic 40/40 reproduction
vs. 0/40 on a live handle, see below), and the retained test now reproduces the
warning through the real listener path.

It also means `Tokens.applyEdit` gets invoked on every keystroke in *every*
open Agda file, not just the relevant one — wasted/wrong work on irrelevant
documents. Note, though, that on current `master` this no longer inflates
`deltas`: `applyEdit` clears `self.deltas` via `rebaseTokens` on every call
(`src/Tokens.res:484`), so the extra invocations don't compound the #318
slowdown the way they might have before that fix landed.

## Current working model

The investigation has moved from reproduction search to fix work. H1-H24 are
the historical search record. H25-H29 reduced the discovery-era scaffolding into
the small permanent surface now on the branch: one focused reproducer, generic
extension-host log lookup through `ExtensionContext.logUri`, and
`AGDA_TEST_GLOB` for focused test runs. H30 is the first fix handoff.

Stale `Decoration.apply` attribution and warning-count scaling remain
historical evidence in the H26/H27 records below, not permanent test
requirements. The reported "hundreds per keypress" flooding is still not
directly observed; the retained reproduction only proves the exact warning on
the real listener path. The individual experiment records below remain the
provenance.

1. **Diagnostics authority — confirmed by H5.**
   Future reproduction attempts must use extension-side evidence:
   extension-side commands, extension-side logs, or returned extension-side
   `State` objects. Direct test-side imports of `Registry` and
   `Singleton.Panel` are not authoritative because H5 proved they observe
   different module instances from the running extension bundle.

2. **Stale-capture shape — confirmed by H6.**
   The core #300 shape is reachable: `State.editor` can move to a current
   editor while the global listener closures registered by `Main.initialize`
   still hold the original captured editor. H6 proved this with
   reference-identity checks (`capturedEditor !== state.editor` and
   `eventEditor === state.editor`). This confirms the routing bug shape, but
   not yet the disposed-editor warning itself.

3. **Disposal lifecycle — same-group disposal falsified by H9; H10 found the
   harness can't reach genuine close.**
   H7 showed that same-document split editor groups are a poor harness:
   active-editor close left the captured editor usable, while group close
   invalidated the test's current-editor handle. H8 pivoted to the source
   code's own clue in `src/Main/Main.res` — ordinary same-group tab
   switching — and confirmed that this alone replaces `state.editor` via
   `Inputs.onOpenEditor` and recreates the stale-capture shape (captured
   editor diverges from the live `state.editor`) without split editor groups,
   though its probe still found `VSCode.TextEditor.document` on the stale
   captured editor succeeding, with no throw and no disposed-editor warning.
   H9 then drove that same same-group harness through delays (up to 3s, then
   a fresh 5s wait), repeated tab churn, and an explicit close/reopen of the
   tab — every condition proposed as the missing ingredient — and the
   captured editor's `document` access kept succeeding throughout, with no
   throw and no disposed-editor warning at any point. That falsifies the
   same-group disposal path outright: in this harness, VS Code simply never
   disposes a captured `TextEditor` handle for a document that stays open in
   the same group, no matter the timing or churn. H10 then tried to drive a
   genuine document/editor close — closing the active editor, closing all
   editors and reopening, dirty-edit-then-close, and a rapid close/probe race —
   and came back **inconclusive**: `Inputs.onCloseDocument` never fired for
   `InputMethod.agda` in any variant, so `onDidCloseTextDocument` itself never
   reaches the extension in this headless single-window harness; the document
   and its state stayed alive and responsive throughout. The remaining gap is
   therefore not a deterministic automated path for reproducing the exact
   warning.

   H11 then ran an automated stale-route diagnostic: it confirmed that on
   master, after H8's deterministic same-group editor replacement, the global
   `onDidChangeTextEditorSelection`/`onDidChangeTextDocument` listeners are the
   same long-lived closures over the originally-captured `editor`, and they
   keep firing — on live, matching events — while `capturedEditor !==
   state.editor`. But the counters it used to test a #304-style routing patch
   measured only "did the closure fire while stale," not "did the closure body
   actually dereference/consume the stale captured editor," so the
   before/after comparison could not distinguish a fix that stops the *use* of
   the stale reference from one that does nothing — see H11's Result for why
   that makes the comparison inconclusive. H12 then added the corrected metric
   and confirmed that the deterministic H8 lifecycle does make the listeners
   dereference/pass the stale captured `editor`. That is still only a
   precursor condition: #300 itself is not reproduced until the harness can
   make that captured `TextEditor` actually closed/disposed and then observe
   the throw or warning.

4. **Event path — selection first, text-change fallback.**
   The selection listener is the shortest signal path because
   `onDidChangeTextEditorSelection` immediately reads
   `VSCode.TextEditor.document(editor)` from the captured editor and has no
   event-document guard. If selection cannot expose the warning, the text
   listener remains a fallback through `IM.Input.fromTextDocumentChangeEvent`,
   `Tokens.applyEdit`, or `Goals.scanAllGoals`, but it is noisier and should
   not be the first probe.

5. **Reproduction status (as of H13-H18) — full candidate surface tested,
   warning not yet reproduced.** This item describes the state of the
   investigation through H18, before H19 first observed the warning
   naturally and H22/H26 later reproduced it deterministically and
   automatically (see items 6, 9, and 13 below) — kept here for the
   provenance of why H19's route was the one tried next.
   The automated evidence available at this stage proved stale captured-editor
   use, but not yet the user-visible #300 warning. H13 searched for a
   deterministic VS Code
   lifecycle that makes any captured `TextEditor` handle become
   closed/disposed in the headless harness, across six lifecycle types and
   their applicable close actions, and found none: every probed handle stayed
   live and resolved its `.document`/`fileName` correctly up to a second after
   closing. Automatic reproduction of #300's disposed-editor warning therefore
   remains blocked because the harness cannot produce any disposed
   `TextEditor` handle through document access alone. H14 should test the gap
   H13 left open: the warning may be operation-specific, appearing only when a
   stale/captured `TextEditor` is used for editor operations such as selection
   mutation, reveal, edit, or decorations, even though `.document` still
   resolves. H14 then found that `TextEditor.edit` is the only probed operation
   that throws on a closed captured editor, but the Agda listener route does
   not call `TextEditor.edit`. H14's listener-trigger phase also did not verify
   that `onDidChangeTextDocument` actually fired. H15 should close that
   evidence gap by instrumenting the real `Main` listeners and forcing a real
   automated text/selection event after the stale closed captured-editor setup.
   H15 verified the stale-but-open listener path: both selection and text-change
   listeners fire while `capturedEditor !== state.editor`, all captured-editor
   operations return `ok`, and no warning appears. H15 did **not** achieve the
   stale-and-edit-closed precondition; based on H10/H13/H14, closing the tab
   appears to tear down the state/listeners that would need to fire. H16 should
   directly measure that close/teardown ordering and race window.
   H16 confirmed that the teardown window exists: after `closeActiveEditor`
   resolves, old global listeners can still fire for at least 50ms while an
   added `TextEditor.edit` probe rejects with
   `"TextEditor#edit not possible on closed editors"`. That is still
   probe-based evidence, not full #300 reproduction, because the real listener
   operations do not call `edit`. H17 should reuse H16's schedule but remove the
   artificial `edit` failure path, instrument the actual listener operations,
   and capture whether they emit the exact reported warning inside the teardown
   window. H17 falsified the warning route for the operations it actually
   reached (`TextEditor.document`, `IM.Input.fromTextDocumentChangeEvent`,
   `Tokens.applyEdit`, `Goals.scanAllGoals`) inside the H16 teardown window:
   all completed without throwing or emitting the warning. But `setDecorations`
   and `revealRange` were not reached (`setDecCount=0`, `revealRangeCount=0`).
   H18 forced both of those remaining UI operations inside the same teardown
   window using real Agda-loaded decoration state (`setDecCount=5`,
   `revealRangeCount=1` across all three schedules). Both ran silently: no
   throw, no warning. The full candidate operation surface is now tested
   inside the H16 close-teardown window, and none of the operations
   reproduces the #300 warning there. (H19, below, later observed the
   warning via a different path: the H8 tab-switch stale-capture route.)

6. **Unicode IM stress — H19 result.**
   H19 ran a 5-phase Unicode IM stress matrix (1 baseline, repeated keypresses,
   repeated activation cycles, tab-switch stress, close-teardown). The exact
   `TextEditor is closed/disposed` warning appeared once in exthost.log, timed
   to phase 4 (H8 tab-switch stale-capture). The correlated path is
   `Tokens.applyDecorations` → `Editor.Decoration.apply` →
   `capturedEditor.setDecorations(...)` on a tab-switch-disposed handle
   (40 decoration calls, all stale). The exthost.log line carries no stack,
   so the assignment is correlational. The "hundreds of warnings per keypress"
   flooding has not been reproduced; only one warning occurred across the full
   938ms test run.

7. **Repeatability and attribution — H20 result.**
   H20 isolated `setDecorations` on a tab-switch-disposed captured editor from
   every other operation (no IM, no `Tokens.applyEdit`, no `Goals.scanAllGoals`):
   it reproduced the exact warning deterministically (40/40 calls across two
   independent runs), against 0/40 on the live editor — confirming the
   mechanism causally for the narrow isolated call. But the naturalistic IM
   workflow itself did not repeat the warning: 10 independent repetitions of
   the H19 recipe plus three long-stress variants (200 cycles; 50 tab-switches
   × 5 cycles; 3 simultaneously-stale states) produced zero further warnings.
   The reported flooding was not reproduced or approached under any of these
   conditions.

8. **Isolated-vs-real-path mismatch — H21 result, corrected by H22.**
   H21 captured decoration payloads from a real IM stress run with no real
   Agda file loaded (0 warnings), replayed those payloads as standalone calls
   on the stale editor (40/40 warned), and concluded the determining factor
   was *surrounding work*: synchronous listener-stack calls never warn,
   decoupled calls always do. H22 found this incomplete: without a real Agda
   load, `Tokens`'s decorations map is empty, so its `Editor.Decoration.apply`
   calls (the ones that use the closure's stale editor) never fire at all —
   every decoration call H21 actually captured came from `IM.res`'s
   underline-candidate decoration, which always reads the *live* `state.editor`
   fresh. H21's comparison was really live-and-synchronous (IM, naturally) vs.
   stale-and-decoupled (the test's own replay commands) — not the same editor
   in both conditions. See H22 below.

9. **Natural synchronous reproduction, no test-only replay needed — H22 result.**
   H22 loaded a real Agda file (giving `Tokens` real, non-empty decorations to
   apply), staled it via an H8 tab switch, then typed: the real
   `onDidChangeTextDocument` listener called `Editor.Decoration.apply` with the
   stale captured editor *synchronously, inside its own call stack* — exactly
   the context H21 said never warns — and produced the exact warning 5/5
   times, reproduced identically across two independent runs. No test-only
   replay or isolation command triggered the decoration call itself; this is
   the first automatic natural-path reproduction of the exact warning through
   the real listener path in the investigation. Flooding was not reproduced: the
   stale-call count stayed fixed at 5 regardless of 100 cycles or 10 repeated
   tab switches. A second, structurally different, unexplained source of the
   exact warning also turned up (10 occurrences with no corresponding
   `Editor.Decoration.apply` call at all) — left open for future work.

10. **Non-decoration warning source — not reproduced in H23.**
    H22 phase E produced 10 exact `TextEditor is closed/disposed` warnings
    during rapid cross-switching among three stale states with no real Agda
    highlighting data and no stale `Editor.Decoration.apply` calls. H23
    reconstructed that phase (H22's own test file no longer existed to replay
    directly) and heavily exercised the leading candidate —
    `Main.onDidChangeTextEditorSelection` reading its captured, possibly stale
    `editor.document` — hundreds of times across two independent runs, plus a
    second, more literal reconstruction, without ever reproducing the warning
    (0 exact warnings across all three fresh-process runs / six reported
    conditions). This weakens but does not falsify the selection-listener
    hypothesis, since the reconstruction could differ from H22's exact recipe
    in some detail, and the "stale-or-unknown" attribution tag used this round
    doesn't prove genuine VS Code-side disposal for a multi-state setup the
    way H8's single-state check did. The phase E warning source remains
    unattributed; see H23 below.

11. **Phase-E rediscovery before attribution — H24 result, not reproduced.**
    H23's strongest result is negative: a deterministic reconstruction of H22
    phase E did not reproduce the warning at all, so attribution gates were
    uninformative. H24 ran a fresh automated search over editor churn shape —
    file counts 3/8/20, two tab-switch primitives, two churn shapes, and
    combined selection/text-change/IM stimulus — across six variants in two
    fresh extension-host processes. None reproduced the warning (0 across all
    six), despite thousands of genuinely stale (per-file confirmed) editor
    touches. Editor-group layout, explicit tab-limit configuration, the
    close/reopen primitive, isolated stimulus levels, and timing delays remain
    untested. Two consecutive rounds (H23 and H24, eight attempted variants
    total) have now failed to rediscover this warning source; a future round
    should weigh continuing the search against accepting it as an open
    question.

12. **Persistent lab infrastructure — H25 complete; consolidated and reduced
    by H28.**
    H20-H24 repeatedly rebuilt the same scaffolding: `TestSuiteAdapter` filters,
    exact extension-host log discovery/counting, temporary issue-300 commands,
    captured/current editor identity tagging, visible-editor/state-count
    telemetry, stale decoration counters, and scenario helpers for tab
    switching, IM cycles, cursor/type/revert stimulus, and fresh-process
    variants. H25 persisted the reusable parts behind an explicit test gate.
    H27 then found that some of this persistence was useful evidence plumbing
    but some was discovery-era structure (H-numbered test names,
    `Issue300Harness`, issue-specific runner environment names) not suited to
    permanent test design. H28 completed the cleanup: `Issue300Harness` and
    `Issue300Probe` are gone entirely (removed from both test and production
    code), `AGDA_TEST_GREP`/`AGDA_TEST_TIMEOUT` were removed, and the remaining
    useful surface was reduced to `AGDA_TEST_GLOB` file selection, a generic
    `Test__Util.ExtHostLog` log helper, and one behavior-named reproduction
    test. H29 then removed the last env-based log-path contract. See the H28
    and H29 Results sections below.

13. **Reproduction consolidation — H26 complete, confirmed deterministic.**
    H22's recipe (real Agda load → H8 tab-switch staling → text-change
    stimulus → real listener calls `Editor.Decoration.apply` with the stale
    editor) was ported onto the H25 infrastructure as a permanent, compact
    test (`test/tests/Test__Issue300H26.res`). Three independent
    `npm test` invocations all reproduced the exact warning, with the warning
    count strictly higher after the IM-cycle stimulus than right after
    staling (proving the stimulus, not setup alone, produces it), and every
    captured `Decoration.apply` call for the loaded file tagged `stale`. This
    is now the default, always-available automatic reproduction harness for
    #300's exact warning on the real listener path — see item 14 and the H26
    Results section below for the per-run data.

14. **Deterministic automatic reproduction — H26 result.**
    #300's exact `TextEditor is closed/disposed` warning is now reproduced
    automatically and deterministically, on demand, via
    `AGDA_ISSUE300_PROBE=1 AGDA_TEST_GLOB="Test__Issue300H26*.js" npm test`:
    three independent fresh-process runs all produced the warning (40, 40,
    and 40 occurrences respectively) strictly after the H8-stale tab switch
    and IM-cycle stimulus, all attributed to stale `Decoration.apply` calls on
    the loaded file's captured editor. What remains open: the reported
    "hundreds of warnings per keypress" flooding behaviour (H26's counts are
    tens per ten IM cycles, not hundreds per keypress), and the structurally
    different, still-unattributed non-decoration warning source from H22
    phase E (H23/H24 could not rediscover it).

15. **Stimulus reduction and scaling — H27 complete.**
    H27 found a smaller deterministic reproducer than H26: a single
    type-then-revert edit (one character typed, then deleted) after the same
    real-load + H8 stale setup reproduces the exact warning, verified
    identically across three independent runs. It also found the warning
    count is *not* bounded/self-limiting the way H22's phases G/H concluded
    for that round's setup: 50 IM cycles (5x H26's 10-cycle stimulus)
    produced 206 warnings, roughly 5x H26's ~40 -- consistent with
    near-linear scaling in stimulus size rather than a fixed cap. A single
    cursor move with no text edit, and a plain delay with no stimulus at all,
    both produced zero warnings, confirming the warning requires an actual
    text-change/IM stimulus on top of the stale setup, not staling alone. See
    the H27 Results section below for the full variant table.

16. **Reproduction-lab consolidation — H28 complete.**
    The permanent test surface is now reduced to `Test__StaleEditorDecorationWarning.res`
    (`AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test`), a
    generic `Test__Util.ExtHostLog` helper, and normal `AGDA_TEST_GLOB`
    file selection -- no probe, no harness module, no H-numbered names in the
    permanent suite. Verified deterministic across three independent runs
    after the consolidation, and the full `npm test` suite passes (one
    unrelated, pre-existing transient timeout in `Test__Connection.res`,
    confirmed non-reproducing on immediate retry). See the H28 Results
    section below for the full classification table.

17. **Log-path plumbing cleanup — H29 complete.**
    `AGDA_TEST_USER_DATA_DIR` was removed. `Test__Util.ExtHostLog` now derives
    `exthost.log` from the activated extension's own `ExtensionContext.logUri`
    (exported as `Main.activationExports.logUri`, cached in `Test__Util` on
    activation), with no environment variable and no `/tmp` globbing. See the
    H29 Results section below.

18. **Fix stale captured-editor routing — H30 complete.**
    Both `Main.initialize` listeners are now document-scoped and the
    `onDidChangeTextDocument` handler uses the current `state.editor` instead
    of the initialization-time captured editor. The retained reproduction
    sequence was inverted into a regression test asserting a zero warning
    delta, and it passes. See the H30 Results section below.

Deprioritized or resolved branches:

- H1-H4 are mostly superseded by H5. Their direct test-side registry/singleton
  observations were either falsified, inconclusive, or invalidated by the
  module-instance mismatch.
- H7 deprioritizes same-document split editor disposal as the main path. It
  can still be useful as a stale-identity diagnostic, but not as the primary
  warning reproduction route.

## Experiment handoffs

### H1: single-editor close lifecycle

Hypothesis: closing the only visible editor for the Agda document also closes
the underlying `TextDocument`.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create a focused diagnostic VS Code integration test. Prefer a temporary
   `it.skip` test in `test/tests/Test__EditorIM.res`, or a new temporary
   `test/tests/Test__Issue300.res` if that is cleaner.
3. Open `test/tests/assets/InputMethod.agda` with the existing
   `activateExtensionAndOpenFile` helper.
4. Capture `editor`, `document`, and `document.fileName`.
5. Register a temporary `VSCode.Workspace.onDidCloseTextDocument` listener.
   Record whether the closed document's `fileName` equals the captured
   `fileName`.
6. Execute `VSCode.Commands.executeCommand0("workbench.action.closeActiveEditor")`.
7. Wait for the close event with a bounded timeout. Do not let the test hang if
   no close event arrives.
8. Record these observations:
   - Did `onDidCloseTextDocument` fire for `InputMethod.agda`?
   - Does `VSCode.Workspace.textDocuments` still contain `InputMethod.agda`, if
     this API is available in the bindings?
   - Does `Registry.get(document)` still find an entry, if it is accessible
     from the test?
9. Interpret the result:
   - `confirmed`: the close event fires for `InputMethod.agda`.
   - `falsified`: the close event does not fire and the document remains live.
   - `inconclusive`: the test cannot reliably observe the document lifecycle.
10. Write the result and raw observations below in this file.
11. Clean up the lab: remove or skip any temporary diagnostic test, remove
    temporary listeners/fixtures, and leave the working tree with only the
    intended documentation update unless a permanent test was explicitly
    requested.

Result:

- Status: **falsified**. Closing the only visible editor for
  `InputMethod.agda` via `workbench.action.closeActiveEditor` does **not**
  close the underlying `TextDocument` — VS Code keeps the document open (e.g.
  as a "preview"/background document) once it has been touched by the
  extension.
- Observations (raw, from the temporary diagnostic test, run on `master`):
  - `onDidCloseTextDocument` fired for `InputMethod.agda`: **no**
  - All closed `fileName`s observed during the 3s bounded wait: **`[]`**
    (empty — no `onDidCloseTextDocument` event fired at all for any document)
  - `InputMethod.agda` still present in `VSCode.Workspace.textDocuments`
    after the close command: **yes**
- Interpretation: H1 is falsified. The single-editor close lifecycle does not
  tear down the `TextDocument` (and therefore would not destroy the `State`
  via `Inputs.onCloseDocument` / `Registry.removeAndDestroy`). This rules out
  H1 as the repro shape. H2 is conditional ("if the document closes, state
  teardown follows") and was not exercised here, since the document never
  closed — it remains an open explanation for cases where the document does
  close, rather than something this experiment confirms or refutes. The
  result is consistent with hypotheses 3–7: the #300 lifecycle likely
  requires a disposed `TextEditor` whose `TextDocument` (and `State`) remain
  alive — e.g. the two-editor-group setup described in hypothesis 5 — rather
  than a simple single-editor close.
- Cleanup: the temporary `test/tests/Test__Issue300.res` diagnostic test and
  its compiled artifacts have been removed (verified via rebuild — no
  `Test__Issue300*` files remain); no permanent test was added. `git status`
  shows only this documentation change (`M docs/issue#300.md`); no other
  files or directories remain in the working tree.

### H2: document-close state teardown

Hypothesis: if the Agda `TextDocument` actually closes, the extension removes
and destroys the associated `State`, so stale listeners from that state cannot
fire afterward.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create a focused temporary diagnostic test. Prefer a temporary `it.skip`
   test in `test/tests/Test__EditorIM.res`, or a new temporary
   `test/tests/Test__Issue300.res` if that is cleaner.
3. Open `test/tests/assets/InputMethod.agda` with
   `activateExtensionAndOpenFile`.
4. Activate or load the extension state so `Registry.get(document)` returns an
   entry with a live `state`.
5. Record these pre-close facts:
   - `Registry.get(document)` is present.
   - The state has subscriptions, if that can be observed safely.
   - `document.fileName`.
6. Register temporary observers:
   - `VSCode.Workspace.onDidCloseTextDocument`, filtered to this `fileName`.
   - Any available registry/log signal that indicates `removeAndDestroy`
     occurred.
7. Force an actual `TextDocument` close. Try the least invasive sequence first:
   - `workbench.action.closeActiveEditor`
   - `workbench.action.closeAllEditors`
   - If no document close occurs, try a fresh copied Agda temp file under
     `/tmp` or the test temp root, open it, initialize state for it, close all
     editors for it, then delete/reopen nothing else.
8. Use a bounded wait after each close attempt. Do not let the test hang.
9. If `onDidCloseTextDocument` fires for the target Agda file, record:
   - whether `Registry.get(document)` is absent afterward.
   - whether later selection/text-change events for other documents produce no
     events from the destroyed state, if this can be observed.
10. Interpret the result:
   - `confirmed`: a real document close fires and the registry entry/state is
     removed.
   - `falsified`: a real document close fires but the registry entry/state
     remains live.
   - `inconclusive`: no reliable way was found to make VS Code close the Agda
     `TextDocument` in the test harness.
11. Write the result and raw observations below in this file.
12. Clean up the lab: remove temporary tests, generated compiled artifacts,
    copied temp files, listeners, and any scratch directories. Leave the
    working tree with only the intended documentation update unless a permanent
    test was explicitly requested.

Result:

- Status: **inconclusive**. The experiment could not exercise the
  document-close teardown path at all, for two independent reasons observed in
  the same run.
- Observations (raw, from the temporary diagnostic test, run on `master` inside
  the shared test suite):
  - `agda-mode.load` result: **`Some(Ok(_))`** (the command reported success).
  - `Registry.getAll()` length immediately after `agda-mode.load` resolved:
    **`0`** — and it stayed **`0`** after both a 100ms and a 1000ms wait.
  - `Registry.get(document)` for `InputMethod.agda` after the (apparently
    successful) load: **`None`** (no live entry to observe).
  - `onDidCloseTextDocument` fired for `InputMethod.agda` after
    `workbench.action.closeActiveEditor`: **no**; after
    `workbench.action.closeAllEditors`: **no**.
  - All closed `fileName`s observed during the bounded waits: **`[]`** (empty).
- Interpretation: two separate obstacles blocked this experiment from
  exercising H2's hypothesis:
  1. **No live registry entry could be observed**, even though `agda-mode.load`
     reported success — `Registry.getAll()` was already `0` immediately after
     the command resolved and stayed `0`. This part is directly observed.
     The *cause* is not directly observed and is only a **candidate/inferred
     explanation**: `initialize` (`src/Main/Main.res:56-58`) wires
     `Registry.removeAndDestroyAll` to fire via `Promise.finally` as soon as
     `WebviewPanel.onceDestroyed(panel)` resolves, and it's plausible that
     inside the shared test suite the `Singleton.Panel` instance is already
     destroyed by the time a later test's `agda-mode.load` runs — tearing down
     the just-added registry entry before the command's promise resolves,
     independent of any document-close event. The lab did not instrument the
     panel-destroy signal itself, so this remains a **likely shared-suite /
     singleton-panel artifact**, not a confirmed mechanism — and not evidence
     about H2 itself either way.
  2. **The document still does not actually close** in this harness, exactly as
     H1 found: `onDidCloseTextDocument` never fired and no `fileName`s were
     observed, regardless of `closeActiveEditor`/`closeAllEditors`. So even with
     a stable live registry entry, this experiment shape could not have reached
     the close-triggered teardown path to confirm or falsify it.
  H2 therefore remains an **untested conditional**: the source
  (`src/Main/Main.res:353-358`, `Inputs.onCloseDocument` →
  `Registry.removeAndDestroy`) plainly wires document-close to registry
  teardown, but neither this experiment nor H1 could force VS Code to actually
  close the `TextDocument` in the integration-test harness, so the conditional
  premise ("if the document closes") is never met. Confirming or falsifying H2
  would require either a different harness capable of truly closing a
  `TextDocument` (e.g. driving VS Code through a separate, isolated extension
  host/profile rather than the shared singleton-activation suite — see
  hypothesis 5's two-editor-group idea as a possible route to a real close), or
  abandoning the close-based shape in favor of hypotheses 3–7's
  disposed-`TextEditor`-with-live-`TextDocument` lifecycle, which does not
  depend on the document ever closing.
- Cleanup: the temporary `test/tests/Test__Issue300.res` diagnostic test and
  its compiled artifacts have been removed (verified via rebuild — no
  `Test__Issue300*` files remain); no permanent test was added, no listeners or
  scratch directories were left behind. `git status` shows only this
  documentation change (`M docs/issue#300.md`); no other files or directories
  remain in the working tree.

### H3: disposed editor with live document

Hypothesis: the #300 warning requires a `TextEditor` object captured by
`initialize` to become disposed while its `TextDocument` remains open and the
associated `State` remains alive.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create a focused temporary diagnostic test. Prefer a temporary `it.skip`
   test in `test/tests/Test__EditorIM.res`, or a new temporary
   `test/tests/Test__Issue300.res` if that is cleaner.
3. Avoid the shared-suite singleton-panel artifact from H2. Either:
   - run this as an isolated diagnostic file if the test runner can be scoped,
     or
   - place the diagnostic early enough that `Singleton.Panel` has not already
     been destroyed, and explicitly record whether a live registry entry exists
     after activation.
4. Open `test/tests/assets/InputMethod.agda` and activate or load the extension
   state so the state is created. Capture:
   - `editorA`
   - `documentA`
   - `fileName`
   - the `state` returned by `agda-mode.load` or `agda-mode.input-symbol[Activate]`
   - whether `Registry.get(documentA)` is present
5. Close the visible editor using `workbench.action.closeActiveEditor`.
6. After the close, record:
   - whether `onDidCloseTextDocument` fired for `fileName`
   - whether `fileName` is still present in `VSCode.Workspace.textDocuments`
   - whether `Registry.get(documentA)` is still present
   - whether `state.document.fileName == fileName`
7. Reopen the same document with `VSCode.Window.showTextDocumentWithUri`.
   Capture the returned `editorB` and `documentB`.
8. Record the identity/lifecycle facts:
   - `editorA === editorB`
   - `documentA === documentB`
   - `state.editor === editorB`, after the activation/open-editor handler has
     had a chance to run
   - whether accessing `editorA.document.fileName` succeeds or throws
   - whether using `editorA` in a harmless editor API, if one exists, succeeds
     or throws
9. Use bounded waits after close/reopen and after active-editor changes. Do not
   let the test hang.
10. Interpret the result:
   - `confirmed`: `editorA` becomes unusable/disposed, the document remains
     live, and the registry/state remains live.
   - `falsified`: `editorA` remains usable, or the document/state does not
     remain live.
   - `inconclusive`: the test cannot reliably observe whether `editorA` is
     disposed.
11. Write the result and raw observations below in this file.
12. Clean up the lab: remove temporary tests, generated compiled artifacts,
    listeners, scratch files, and any copied temp files. Leave the working tree
    with only the intended documentation update unless a permanent test was
    explicitly requested.

Result:

- Status: **partially falsified / inconclusive for the full hypothesis**. The
  editor-disposal subclaim was falsified for this close/reopen shape:
  `editorA.document.fileName` remained accessible and returned the correct,
  live file name, so `editorA` does not become disposed/unusable here. But the
  full H3 lifecycle — "captured `TextEditor` becomes disposed **while its
  `TextDocument` remains open and the associated `State` remains alive**" —
  was not actually exercised, because the `State` precondition from step 4 was
  never met: `Registry.get(documentA)` was already `None` immediately after
  `agda-mode.load`, and no live `State` was observed at any point. So this
  experiment falsifies only the narrower claim "close/reopen disposes
  `editorA` in this harness," not the full H3 hypothesis as stated.

- Observations (raw, from the temporary diagnostic test, run on `master`
  inside the shared Mocha suite):
  - `agda-mode.load` result: `Some(Ok(_))`
  - `Registry.get(documentA)` present after load: **no**
  - live `State` present after load: **no**
  - `onDidCloseTextDocument` fired for `fileName`: **no**
  - all closed file names observed: *(none)*
  - `fileName` still present in `VSCode.Workspace.textDocuments` after close:
    **yes**
  - `Registry.get(documentA)` present after close: **no**
  - live `State` present after close: **no**
  - `editorA === editorB` (after closing and reopening the document): **no**
  - `documentA === documentB`: **yes**
  - accessing `editorA.document.fileName` after close/reopen: **succeeded**,
    returned `.../test/tests/assets/InputMethod.agda`

- Interpretation:
  1. **`editorA` does not become disposed/unusable for this close/reopen
     shape.** This is a directly observed fact, not an inference: calling
     `editorA.document.fileName` after the editor was closed and the document
     reopened succeeded and returned the correct file name. A disposed/invalid
     `TextEditor` would be expected to throw on property access; it did not.
     This falsifies the narrower "close/reopen disposes `editorA`" subclaim,
     but is not by itself a verdict on the full H3 hypothesis (see point 3).
  2. **The reopened editor is a distinct object, but the document is the
     same.** `editorA === editorB` is `no` while `documentA === documentB` is
     `yes` — VS Code hands out a fresh `TextEditor` wrapper for the same
     underlying `TextDocument` when the file is reopened. So "the editor
     becomes stale/inactive" and "the editor becomes disposed" are different
     things; only the former is observed here.
  3. **The registry/state precondition from step 4 was not met**, exactly as
     in [[H2]]: `Registry.get(documentA)` was already `None` immediately after
     `agda-mode.load`, so no live `State` was ever observed, and
     `state.editor === editorB` / `state.document.fileName == fileName` could
     not be checked. This is the same likely shared-suite / singleton-panel
     artifact noted in H2's interpretation — a candidate/inferred explanation,
     not directly observed here either. Because H3 is specifically a claim
     about `editorA` becoming disposed *while the `State` remains alive*, the
     missing `State` means the full hypothesis was never actually exercised —
     only the disposal subclaim could be checked in isolation, and that part
     was falsified.

- Cleanup: the temporary `test/tests/Test__Issue300.res` diagnostic test and
  its compiled artifacts (including stray `.cmt`/`.cmj` files under `lib/bs/`)
  were removed after the run; `git status` shows only the documentation change
  to this file.

### H4: live state after activation

Hypothesis: a useful #300 repro must run in a harness where activating/loading
an Agda document leaves a live `State` in `Registry`; the shared Mocha-suite
diagnostics used for H2/H3 may be invalid because the singleton panel teardown
removes the state immediately.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create a focused temporary diagnostic test. Prefer a new temporary
   `test/tests/Test__Issue300.res` so it is easy to include/exclude.
3. Run the diagnostic in the most isolated way available:
   - first try placing it early in the loaded test order, if test order can be
     controlled cheaply;
   - if not, temporarily modify the test adapter/glob pattern locally to run
     only `Test__Issue300`;
   - record exactly which isolation method was used.
4. Open `test/tests/assets/InputMethod.agda` using
   `activateExtensionAndOpenFile`.
5. Execute `agda-mode.load` or `agda-mode.input-symbol[Activate]` and capture:
   - the command result
   - the returned `state`, if present
   - `document.fileName`
6. Immediately after the command resolves, record:
   - `Registry.getAll()->Array.length`
   - whether `Registry.get(document)` is present
   - whether the returned `state.document.fileName` matches `document.fileName`
   - whether `state.subscriptions->Array.length` is nonzero, if safely
     observable
7. Wait 100ms and 1000ms, then record the same registry/state facts again.
8. Instrument or observe the suspected teardown if possible:
   - record whether `Singleton.Panel` / `WebviewPanel.onceDestroyed` appears to
     fire during the diagnostic, if a local temporary hook can be added cheaply;
   - otherwise explicitly record that the panel-destroy cause remains
     uninstrumented.
9. Do not test editor disposal yet. This experiment is only about whether a
   live state can be established and kept alive long enough for later
   lifecycle experiments.
10. Interpret the result:
   - `confirmed`: a live registry entry/state exists immediately after
     activation and remains live through the bounded waits.
   - `falsified`: even in the isolated diagnostic shape, no live registry
     entry/state can be observed after activation.
   - `inconclusive`: test isolation could not be achieved or the observations
     conflict.
11. Write the result and raw observations below in this file.
12. Clean up the lab: revert temporary test adapter/glob changes, remove
    temporary diagnostics, generated compiled artifacts, listeners, scratch
    files, and any local instrumentation. Leave the working tree with only the
    intended documentation update unless a permanent test was explicitly
    requested.

Result:

- Status: **inconclusive — diagnostic invalid for `Registry`/`Singleton`
  assertions**. The `falsified` verdict is not supported: `agda-mode.load`
  returning `Some(Ok(state))` is itself proof that the *extension-side* command
  path found a live `State` via `Registry.get(document)` before returning
  (`src/Main/Main.res:413` only produces `Some(Ok(state))` after
  `Registry.get(document)->Option.flatMap(entry => entry.state)` succeeds). So
  a live registry entry demonstrably existed on the extension side at the
  moment the command resolved — directly contradicting the "no live registry
  entry/state can be observed" reading the diagnostic appeared to support. The
  most likely explanation is a **module-instance mismatch**: the diagnostic
  test imports `src/Registry.res` (and `src/View/Singleton.res`) compiled into
  the *test* bundle (`lib/js/test/...`), while `agda-mode.load` runs inside the
  *extension* bundle (`./dist/app.bundle.js`, built separately by webpack per
  `package.json`). These can be two distinct module instances with two
  distinct `dict`/`ref` singletons, in which case `Registry.getAll()` and
  `Singleton.Panel.get()` observed from the test side say nothing about the
  state of the extension-side `Registry`/`Singleton.Panel` — the `0` and
  `None` readings would be artifacts of reading the wrong instance, not
  evidence that the extension's registry/panel were empty.

- Observations (raw, from the temporary diagnostic test, run in isolation —
  the only test file loaded by the adapter):
  - `Singleton.Panel` populated *before* activation: **no**
  - `agda-mode.load` result: `Some(Ok(_))`
  - `document.fileName`: `.../test/tests/assets/InputMethod.agda`
  - returned `state.document.fileName == document.fileName`: **yes**
  - returned `state.subscriptions` length: **5**
  - immediately after `agda-mode.load` — `Registry.getAll()` length: **0**;
    `Registry.get(document)` present: **no**; live `State` present: **no**
  - after 100ms — `Registry.getAll()` length: **0**; `Registry.get(document)`
    present: **no**; live `State` present: **no**
  - after 1000ms — `Registry.getAll()` length: **0**; `Registry.get(document)`
    present: **no**; live `State` present: **no**
  - `Singleton.Panel` populated *after* activation: **no**
  - `Singleton.Panel` identity unchanged (before `===` after): **n/a (both
    none)**

- Interpretation:
  1. **A live `State` genuinely is created and is reachable through the
     extension-side `Registry` at command-resolution time.** This is directly
     observed, and more strongly than the raw "returned state looked live"
     reading: `agda-mode.load`'s `Some(Ok(state))` result is *constructed from*
     a successful `Registry.get(document)` lookup inside `Main.res` (line 413),
     so the extension-side registry held a live entry for that document at
     that moment. `state.document.fileName` matching and `state.subscriptions`
     having length 5 corroborate that this is a real, populated `State`.
  2. **The diagnostic's own `Registry.getAll()`/`Registry.get(document)`/
     `Singleton.Panel.get()` reads are not trustworthy evidence about the
     extension-side registry or panel.** Given point 1, the `length: 0` /
     `present: no` / `None` readings from the test side cannot mean the
     extension-side registry and panel were actually empty — they more likely
     reflect the diagnostic reading a *different* compiled instance of
     `Registry`/`Singleton` than the one `Main.initialize` populates. This is
     the leading new hypothesis (module-instance mismatch between the test
     bundle and the extension bundle), not a confirmed registry-emptying bug.
  3. **The H2 "singleton-panel teardown" explanation is therefore neither
     confirmed nor ruled out by this experiment.** The previous write-up's
     claim that H4 "rules out" singleton-panel teardown rested on
     `Singleton.Panel.get()` returning `None` from the test side — which, per
     point 2, may simply be the wrong module instance and proves nothing about
     the extension-side panel. That candidate explanation remains exactly
     where H2 left it: an untested, inferred possibility.
  4. **What this experiment actually establishes**, narrowly and reliably: the
     activation command path (`Main.res:413`) does find a live `State` via its
     own `Registry.get(document)` and returns it — so *extension-side*
     activation and registry population work as expected. The open question is
     how to *observe* the extension-side `Registry`/`Singleton.Panel` from a
     test, since importing `src/Registry.res`/`src/View/Singleton.res`
     directly may not reach the same module instance the running extension
     uses.

- Cleanup: the temporary `test/tests/Test__Issue300.res` diagnostic test, its
  compiled artifacts (including `lib/bs/test/tests/Test__Issue300-AgdaModeVscode.cmt`
  and `lib/js/test/tests/Test__Issue300.bs.js`), and the temporary glob-pattern
  change to `test/TestSuiteAdapter.res` (restored to `**/*.js` and rebuilt)
  were all removed/reverted after the run; `git status` shows only the
  documentation change to this file.

### H5: extension-side diagnostics

Hypothesis: the H2-H4 registry/singleton contradictions are caused by the test
bundle and extension bundle using different module instances. A reliable #300
repro needs extension-side diagnostics for registry state, singleton panel
state, and editor identity.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create a focused temporary diagnostic test and temporary extension-side
   instrumentation. Prefer `test/tests/Test__Issue300.res` plus one small
   extension-side diagnostic surface.
3. Do **not** use direct test imports of `Registry`, `Singleton.Panel`, or other
   singleton modules as evidence about extension-side state.
4. Add one temporary extension-side observation mechanism. Acceptable options:
   - a temporary `agda-mode.issue300-dump-state` command registered from
     `Main.activateWithoutContext`;
   - temporary events emitted on `channels.log` from `Registry.add`,
     `Registry.removeAndDestroy`, `Registry.removeAndDestroyAll`, and
     `Main.initialize`;
   - temporary fields returned through the existing command result `State.t`,
     if that is enough to answer the questions below.
5. Open `test/tests/assets/InputMethod.agda` and execute `agda-mode.load` or
   `agda-mode.input-symbol[Activate]`.
6. From the extension-side diagnostic surface, record immediately after command
   resolution:
   - whether the extension-side `Registry` contains `document.fileName`
   - the extension-side registry size
   - whether `Singleton.Panel` is populated extension-side
   - whether `WebviewPanel.onceDestroyed(panel)` has fired, if instrumented
   - the returned `state.document.fileName`
   - `state.subscriptions` length
7. Repeat the extension-side dump after 100ms and 1000ms.
8. If extension-side registry state remains live, perform one minimal lifecycle
   action:
   - close and reopen the editor for the same document;
   - then dump extension-side state again.
9. Record only extension-side observations as authoritative. Test-side imports
   may be recorded separately as a control, but they must be labeled
   non-authoritative.
10. Interpret the result:
   - `confirmed`: extension-side registry/panel state differs from test-side
     direct imports, proving module-instance mismatch; extension-side state can
     be observed reliably.
   - `falsified`: extension-side diagnostics match test-side direct imports,
     and no module-instance mismatch is present.
   - `inconclusive`: instrumentation cannot distinguish extension-side state
     from test-side state.
11. Write the result and raw observations below in this file.
12. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, compiled artifacts, listeners, scratch files, and any test
    adapter changes. Leave the working tree with only the intended
    documentation update unless a permanent diagnostic was explicitly
    requested.

Result:

- Status: **confirmed**.
- Observations: A temporary `agda-mode.issue300-dump-state` command was
  registered from `Main.activateWithoutContext`, reading `Registry.getAll()`,
  `Registry.get(document)`, `Singleton.Panel.get()`, and the live `State.t`
  directly from within the extension bundle, and logging the results via
  `Js.log` (captured in the same extension-host stdout as the test). The test
  opened `InputMethod.agda`, ran `agda-mode.load`, and then triggered the dump
  command immediately, after 100ms, and after 1000ms. Raw output:
  ```
  [issue#300 H5] agda-mode.load result: Some(Ok(_))
  [issue#300 H5 control] test-side Registry.getAll() length: 0
  [issue#300 H5 control] test-side Registry.get(document) present: no
  [issue#300 H5 control] test-side Singleton.Panel populated: no
  [issue#300 H5] dumping extension-side state immediately after load
  [issue#300 H5 ext] registry size: 1
  [issue#300 H5 ext] registry has active document: yes
  [issue#300 H5 ext] Singleton.Panel populated: yes
  [issue#300 H5 ext] state.document.fileName: .../test/tests/assets/InputMethod.agda
  [issue#300 H5 ext] state.subscriptions length: 5
  [issue#300 H5] dumping extension-side state after 100ms
  [issue#300 H5 ext] registry size: 1
  [issue#300 H5 ext] registry has active document: yes
  [issue#300 H5 ext] Singleton.Panel populated: yes
  [issue#300 H5 ext] state.document.fileName: .../test/tests/assets/InputMethod.agda
  [issue#300 H5 ext] state.subscriptions length: 5
  [issue#300 H5] dumping extension-side state after 1000ms
  [issue#300 H5 ext] registry size: 1
  [issue#300 H5 ext] registry has active document: yes
  [issue#300 H5 ext] Singleton.Panel populated: yes
  [issue#300 H5 ext] state.document.fileName: .../test/tests/assets/InputMethod.agda
  [issue#300 H5 ext] state.subscriptions length: 5
  ```
  For the exact same document, at the exact same moments in time, the
  extension-side surface consistently observed a live `Registry` entry
  (size 1, containing the active document, with a live `State` whose
  `document.fileName` matches and 5 subscriptions) and a populated
  `Singleton.Panel`, while the test-side direct imports (recorded as a
  non-authoritative control immediately after `agda-mode.load` resolved)
  simultaneously showed registry size 0, no entry for the document, and no
  populated panel. The registry/state remained live through all three dumps,
  so the optional close/reopen lifecycle step (step 8) was not required to
  produce a definitive answer.
- Interpretation: This is a direct, reproducible divergence between the
  extension bundle's module state and the test bundle's module state for the
  identical document and identical timing — proving the module-instance
  mismatch identified as the leading hypothesis after H4. It also shows that
  extension-side `Registry`/`Singleton.Panel`/`State` can be observed reliably
  through a command registered from within the extension itself. This
  retroactively explains the "always empty registry" readings in H2-H4: those
  experiments were reading the wrong module instance, not observing genuine
  extension behaviour. Conclusions drawn from H2-H4's direct `Registry`/
  `Singleton.Panel` imports about teardown/lifecycle should be treated as
  artifacts of this mismatch, not evidence about the real extension state.
- Cleanup: removed the temporary `agda-mode.issue300-dump-state` command and
  its `Js.log` instrumentation from `src/Main/Main.res`, deleted
  `test/tests/Test__Issue300.res`, restored `test/TestSuiteAdapter.res` from
  backup (and removed the backup), rebuilt, and removed scratch files in
  `/tmp`. Working tree left with only this documentation update.

### H6: extension-side current-editor replacement

Hypothesis: after a close/reopen or equivalent editor replacement lifecycle,
the extension-side `State.editor` points at the surviving/current editor while
the global listener closures registered by `Main.initialize` still hold the
original captured editor.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create one focused temporary diagnostic test, preferably
   `test/tests/Test__Issue300.res`, and temporary extension-side
   instrumentation in `src/Main/Main.res`.
3. Do **not** use direct test imports of `Registry`, `Singleton.Panel`, or
   singleton modules as authoritative evidence. H5 proved those imports can
   observe the wrong module instance.
4. Add temporary extension-side logging inside the relevant `Main.initialize`
   closures. The useful log points are:
   - when `initialize` captures its original `editor`;
   - when the state is updated by the existing open/active-editor path, if a
     local hook can be added there without changing behavior;
   - at the start of `onDidChangeTextEditorSelection`, before any existing
     access that uses the captured `editor`;
   - optionally at the start of `onDidChangeTextDocument`, if the selection
     event does not fire reliably.
5. Each listener log should record, from extension-side code:
   - captured editor `document.fileName`;
   - current `state.editor.document.fileName`;
   - event editor/document `fileName`, if available;
   - whether `capturedEditor === state.editor`;
   - whether `eventEditor === state.editor`, for selection events;
   - whether `event.document.fileName === state.document.fileName`, for text
     change events;
   - whether the extension-side registry still contains `state.document`.
6. Open `test/tests/assets/InputMethod.agda`, execute `agda-mode.load`, and
   keep the returned `State.t` as a control only for document/editor identity.
   Treat the extension-side logs as authoritative.
7. Record the baseline immediately after load:
   - extension-side registry has the document;
   - captured editor file;
   - `state.editor` file;
   - `capturedEditor === state.editor`.
8. Create the editor replacement lifecycle:
   - first try `workbench.action.closeActiveEditor`, reopen
     `InputMethod.agda`, and wait briefly for active-editor handling;
   - if that does not make `state.editor` differ from the captured editor,
     try opening the same document in a second editor group or otherwise
     making a second visible editor for the same document active.
9. Trigger one minimal selection change in the surviving/current editor. If no
   selection event is observed, trigger one minimal text edit and undo it.
10. Record the extension-side listener log emitted by the event.
11. Interpret the result:
    - `confirmed`: the extension-side log shows a live `State` whose
      `state.editor` is the surviving/current editor, while the listener's
      captured editor differs from `state.editor`.
    - `falsified`: the reachable lifecycle never makes
      `capturedEditor !== state.editor`, or `state.editor` does not move to
      the surviving/current editor.
    - `inconclusive`: the harness cannot trigger the replacement lifecycle or
      cannot emit the listener identity log.
12. Write the result, raw observations, and interpretation below in this file.
13. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, compiled artifacts, listeners, scratch files, and any test
    adapter changes. Leave the working tree with only the intended
    documentation update unless a permanent diagnostic was explicitly
    requested.

Result:

- Status: **confirmed**.
- Observations: Temporary `Js.log` instrumentation (prefix
  `[issue#300 H6 ext]`) was added at the start of the `onDidChangeTextEditorSelection`
  and `onDidChangeTextDocument` closures registered in `Main.initialize`
  (`src/Main/Main.res:114` and `:126`), logging the captured `editor`'s
  `document.fileName`, `state.editor`'s `document.fileName`, the event's
  editor/document `fileName`, and three identity checks:
  `capturedEditor === state.editor`, `eventEditor === state.editor` (selection
  events), and `event.document.fileName === state.document.fileName` (text
  change events), plus whether the extension-side registry still contains
  `state.document`.

  The test opened `InputMethod.agda` (capturing editor A via
  `agda-mode.load`), then opened the same document beside it
  (`viewColumn: Beside`), producing a second, distinct `TextEditor.t` (editor
  B; `editorA === editorB` was `no`). It then drove cursor moves
  (`cursorDown`/`cursorRight`), `setSelection`, and a minimal insert+undo edit
  in editor B. Raw extension-side log excerpts:
  ```
  [issue#300 H6 ext] selection: capturedEditor === state.editor: no
  [issue#300 H6 ext] selection: eventEditor === state.editor: yes
  [issue#300 H6 ext] selection: registry has state.document: yes
  ...
  [issue#300 H6 ext] textChange: capturedEditor === state.editor: no
  [issue#300 H6 ext] textChange: event.document.fileName === state.document.fileName: yes
  [issue#300 H6 ext] textChange: registry has state.document: yes
  ```
  (Note: `document.fileName` strings were identical for editor A, editor B,
  `state.editor`, and the event in every log line, since both editors show the
  same file — so filename comparisons alone cannot distinguish the editors;
  the `===` reference-identity checks are the load-bearing evidence here.)

  Across every selection-change and text-change event observed,
  `capturedEditor === state.editor` was consistently `no`, while
  `eventEditor === state.editor` was `yes` for the selection events fired
  directly in editor B (lines reporting `eventEditor === state.editor: yes`
  correspond to the `cursorDown`/`setSelection` actions taken in editor B,
  the surviving/current editor). The extension-side registry continued to
  contain `state.document` throughout.
- Interpretation: This directly confirms the hypothesis. After opening a
  second editor for the same document, the live `State.editor` moved to
  point at the surviving/current editor (editor B) — matching the editor that
  fired the observed events (`eventEditor === state.editor: yes`) — while the
  global listener closures registered by `Main.initialize` still hold the
  *original* captured editor (editor A), and `capturedEditor !== state.editor`
  on every fired event. This confirms that the stale-capture shape described
  by hypothesis #6/#7 — `state.editor` pointing at a different, surviving
  editor than the one captured by the listener closures — is reachable, and
  reachable simply by opening a second editor for the same document; no
  close/reopen lifecycle was even necessary to produce the divergence. The
  listener closures read/operate on `editor` (editor A), not on `state.editor`
  (editor B), which is the shape that *would* lead to "TextEditor is
  closed/disposed" warnings if editor A were later closed/disposed while the
  closures still reference it. However, H6 does not itself prove that editor A
  becomes disposed, nor does it capture the actual disposed-editor warning —
  a later experiment still needs to close/dispose editor A (or otherwise
  observe the live warning) to close that final link in the chain.
- Cleanup: removed the temporary `Js.log` instrumentation from the
  `onDidChangeTextEditorSelection` and `onDidChangeTextDocument` closures in
  `src/Main/Main.res`, deleted `test/tests/Test__Issue300.res`, restored
  `test/TestSuiteAdapter.res` from backup (and removed the backup), rebuilt,
  removed the stray `lib/bs/test/tests/Test__Issue300-AgdaModeVscode.cmt`
  artifact, and removed scratch files in `/tmp`. Working tree left with only
  this documentation update.

### H7: dispose captured editor, then trigger selection in current editor

Hypothesis: after the H6 editor A/editor B divergence is established, closing
or otherwise disposing editor A while editor B and the extension-side `State`
remain live will make a selection event in editor B hit the stale captured
editor A inside `onDidChangeTextEditorSelection`, producing the live
"TextEditor is closed/disposed" warning or an equivalent extension-side
disposed-editor access signal.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create one focused temporary diagnostic test, preferably
   `test/tests/Test__Issue300.res`, and minimal temporary instrumentation in
   `src/Main/Main.res`.
3. Reuse the H6 setup:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load` so `Main.initialize` captures editor A;
   - open the same document beside it to create editor B;
   - confirm from extension-side logs that
     `capturedEditor !== state.editor` and `eventEditor === state.editor` for
     editor B before attempting disposal.
4. Add temporary extension-side logging around the first captured-editor access
   in `onDidChangeTextEditorSelection`. The log should record:
   - before/after attempting `VSCode.TextEditor.document(editor)` on the
     captured editor;
   - whether the call throws/rejects, if the binding exposes that;
   - captured editor file name if accessible;
   - `state.editor` file name;
   - whether `capturedEditor === state.editor`;
   - whether `eventEditor === state.editor`;
   - whether the extension-side registry still contains `state.document`.
5. Also capture the extension-host console/output stream if the test harness
   can observe it, and search only for the relevant disposed-editor text, such
   as `TextEditor is closed` or `TextEditor is disposed`.
6. Dispose editor A while keeping editor B active. Try the smallest lifecycle
   first:
   - focus editor A explicitly, then run `workbench.action.closeActiveEditor`;
   - refocus editor B and wait briefly for active-editor handling.
7. If editor A is still usable after that close, try one or more stronger
   lifecycle variants, recording each attempt separately:
   - close editor A's editor group while editor B remains open;
   - move editor A to a temporary group, close that group, then refocus editor
     B;
   - use any VS Code API exposed by the bindings that directly hides/closes
     the specific editor A without closing the document.
8. After each disposal attempt, trigger one minimal selection change in editor
   B (`cursorDown`, `cursorRight`, or `setSelection`).
9. Record raw observations for each variant:
   - which close/dispose action was used;
   - whether editor B remained active/current;
   - whether extension-side registry still contained the document;
   - whether `capturedEditor !== state.editor` still held;
   - whether the captured-editor document access succeeded, threw, or emitted
     a disposed-editor warning;
   - any exact warning text observed.
10. Interpret the result:
    - `confirmed`: after editor A is closed/disposed and editor B remains live,
      a selection event in editor B causes the captured-editor access to throw
      or emit the disposed-editor warning.
    - `falsified`: editor A can be closed/disposed while editor B and state
      remain live, but selection events in editor B do not trigger any
      disposed-editor access signal.
    - `inconclusive`: the harness cannot actually dispose editor A while
      keeping editor B and the extension-side state live, or cannot observe
      the warning/throw path.
11. Write the result, raw observations, and interpretation below in this file.
12. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, compiled artifacts, listeners, scratch files, and any test
    adapter changes. Leave the working tree with only the intended
    documentation update unless a permanent diagnostic was explicitly
    requested.

Result:

- Status: **inconclusive**.
- Observations: Temporary `Js.log` instrumentation (prefix
  `[issue#300 H7 ext]`) was added at the start of
  `onDidChangeTextEditorSelection` (`src/Main/Main.res:114`), logging
  `capturedEditor === state.editor`, `eventEditor === state.editor`, whether
  the registry still contains `state.document`, and wrapping the first
  captured-editor access (`VSCode.TextEditor.document(editor)`) in a
  success/throw probe.

  The test reproduced the H6 setup (editor A via `agda-mode.load`, editor B
  opened beside it; `editorA === editorB` was `no`), then attempted two
  disposal lifecycles on editor A while probing editor B with cursor moves,
  `setSelection`, edits, and undo (each probe wrapped to catch and log
  success/throw without aborting the run):

  - **Attempt 1** — focus editor A's group (`viewColumn: One`), run
    `workbench.action.closeActiveEditor`, then refocus editor B
    (`viewColumn: Beside`) and probe. All probe actions
    (`cursorRight`, `setSelection`, `edit`, `undo`) **succeeded**. Several
    selection events fired and were logged:
    ```
    [issue#300 H7 ext] selection: capturedEditor === state.editor: no
    [issue#300 H7 ext] selection: eventEditor === state.editor: no
    [issue#300 H7 ext] selection: registry has state.document: yes
    [issue#300 H7 ext] selection: about to access captured editor's document
    [issue#300 H7 ext] selection: captured editor document access succeeded, fileName: .../InputMethod.agda
    ...
    [issue#300 H7 ext] selection: eventEditor === state.editor: yes
    ```
    `capturedEditor === state.editor` was `no` on every event (consistent
    with H6), `eventEditor === state.editor` alternated `yes`/`no` depending
    on which editor fired the event, the registry still contained
    `state.document`, and **the captured-editor document access always
    succeeded — no throw, no disposed-editor warning** was observed for this
    lifecycle.
  - **Attempt 2** — focus editor A's group again, run
    `workbench.action.closeEditorsInGroup`, then refocus editor B and probe.
    `cursorRight` and `setSelection` succeeded and produced **no**
    `[issue#300 H7 ext]` log lines (no selection event fired this time), and
    then:
    ```
    [issue#300 H7] probe "edit editorB (insert h7b)" THREW
    ```
    The thrown error (observed directly in an earlier, non-probe-wrapped run
    of the same lifecycle) was `Error: TextEditor#edit not possible on closed
    editors` — raised from the test's own `editorB->VSCode.TextEditor.edit`
    call, i.e. the test-side `editorB` handle itself had become unusable, not
    the extension's captured `editor` (editor A). `undo` afterward still
    succeeded.
- Interpretation: The harness could not realize the intended lifecycle —
  "editor A disposed, editor B and `State` remain live, then a selection
  event in editor B drives the stale captured-editor access in
  `onDidChangeTextEditorSelection`." Closing editor A's *group* (attempt 2)
  ended up invalidating the test's `editorB` handle as well (VS Code appears
  to treat same-document editors opened via `showTextDocumentWithUri` in
  different view columns as sharing or rapidly recycling underlying editor
  resources, rather than keeping fully independent, individually-disposable
  `TextEditor` instances reachable from the test side), so the harness cannot
  cleanly keep "the other one" alive while disposing one specific editor for
  a shared document. Closing only the *active editor* (attempt 1) left both
  editor A and editor B functional — the captured-editor access kept
  succeeding, so no warning could be produced from that weaker lifecycle
  either. Per the spec's classification criteria this is **inconclusive**:
  "the harness cannot actually dispose editor A while keeping editor B and
  the extension-side state live, or cannot observe the warning/throw path."
  No extension-side disposed-editor warning or throw was ever observed on the
  captured-editor (`editor` / editor A) access path in either lifecycle
  variant tried.
- Cleanup: removed the temporary `Js.log`/probe instrumentation from
  `onDidChangeTextEditorSelection` in `src/Main/Main.res`, deleted
  `test/tests/Test__Issue300.res`, restored `test/TestSuiteAdapter.res` from
  backup (and removed the backup), rebuilt both the ReScript output and the
  webpack bundle, removed any stray `lib/bs/.../Test__Issue300*` compiled
  artifacts, and removed scratch files in `/tmp`. Working tree left with only
  this documentation update.

### H8: tab-switch replacement lifecycle

Hypothesis: the disposed-editor warning requires the original tab-switch
lifecycle described in `src/Main/Main.res`: after switching away from and back
to an Agda document, VS Code gives the extension a replacement `TextEditor`
for the same `TextDocument`; `State.editor` is updated to that replacement by
`Inputs.onOpenEditor`, but the global listener closures still hold the
original captured editor, which may now be closed/disposed.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create one focused temporary diagnostic test, preferably
   `test/tests/Test__Issue300.res`, and minimal temporary instrumentation in
   `src/Main/Main.res`.
3. Do **not** use same-document split editor groups as the primary lifecycle.
   H7 showed that route is unreliable for independently disposing editor A
   while preserving editor B. Use ordinary tab switching in a single editor
   group first.
4. Add temporary extension-side logs in two places:
   - inside `Inputs.onOpenEditor` where `state.editor = editor` is assigned;
   - at the start of `onDidChangeTextEditorSelection`, around the first
     `VSCode.TextEditor.document(editor)` access on the captured editor.
5. The `Inputs.onOpenEditor` log should record:
   - opened editor document `fileName`;
   - previous `state.editor` document `fileName`, if accessible;
   - whether previous `state.editor === editor`;
   - whether the extension-side registry contains the document;
   - a monotonically increasing diagnostic counter or label so each
     replacement can be ordered in the raw output.
6. The selection-listener log should record:
   - before/after attempting `VSCode.TextEditor.document(editor)` on the
     captured editor;
   - whether that captured-editor document access succeeds or throws;
   - captured editor file name if accessible;
   - current `state.editor` file name;
   - whether `capturedEditor === state.editor`;
   - whether `eventEditor === state.editor`;
   - whether the extension-side registry still contains `state.document`;
   - any exact disposed-editor warning text captured from the extension host,
     if observable.
7. Test lifecycle A:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load` so `Main.initialize` captures editor A;
   - open a different file in the same editor group, preferably another Agda
     file if available, otherwise any text fixture;
   - switch back to the original `InputMethod.agda` tab using ordinary VS Code
     tab navigation or `showTextDocumentWithUri` without `Beside`;
   - wait briefly for `Inputs.onOpenEditor` to run.
8. After switching back, trigger a minimal selection change in the active
   `InputMethod.agda` editor (`cursorDown`, `cursorRight`, or `setSelection`).
9. Record raw observations:
   - whether `Inputs.onOpenEditor` ran for `InputMethod.agda` after the tab
     switch;
   - whether it replaced `state.editor` with a different editor reference;
   - whether the extension-side registry still contained `state.document`;
   - whether the selection event's editor matched the current `state.editor`;
   - whether the listener's captured editor differed from `state.editor`;
   - whether the captured-editor document access succeeded, threw, or emitted
     a disposed-editor warning.
10. If lifecycle A does not replace `state.editor`, try lifecycle B:
    - open `InputMethod.agda`;
    - open two or more other tabs in the same group;
    - switch through them using `workbench.action.nextEditor` /
      `workbench.action.previousEditor` or equivalent tab commands;
    - switch back to `InputMethod.agda`, then repeat the selection probe.
11. If neither lifecycle replaces `state.editor`, try lifecycle C:
    - open `InputMethod.agda`;
    - open a different file in the same group;
    - close the visible `InputMethod.agda` tab while the different file is
      active or adjacent;
    - reopen `InputMethod.agda` in the same group;
    - repeat the selection probe.
12. Interpret the result:
    - `confirmed`: tab switching or same-group reopen replaces
      `state.editor`, keeps extension-side state live, and a selection event in
      the replacement editor causes the captured-editor access to throw or emit
      the disposed-editor warning.
    - `partially confirmed`: tab switching or same-group reopen replaces
      `state.editor` and recreates the H6 stale-capture shape without using
      split editor groups, but the captured editor still does not throw or emit
      the warning.
    - `falsified`: the harness can replace `state.editor` through tab
      switching while keeping state live, but repeated selection probes never
      produce any disposed-editor access signal.
    - `inconclusive`: the harness cannot get VS Code to replace
      `state.editor` through tab switching/same-group reopen, or cannot observe
      the warning/throw path.
13. Write the result, raw observations, and interpretation below in this file.
14. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, compiled artifacts, listeners, scratch files, and any test
    adapter changes. Leave the working tree with only the intended
    documentation update unless a permanent diagnostic was explicitly
    requested.

Result:

- Status: **partially confirmed**.
- Observations: Temporary `Js.log` instrumentation (prefix
  `[issue#300 H8 ext]`) was added at two points in `src/Main/Main.res`: (1)
  inside `Inputs.onOpenEditor`'s registry-state branch, just before
  `state.editor = editor`, logging a counter, the opened document's
  `fileName`, the previous `state.editor`'s `fileName`,
  `prevEditor === newEditor`, and whether the registry still has the document;
  (2) at the start of the `onDidChangeTextEditorSelection` closure in
  `initialize` (which captures `editor`, i.e. editor A), logging whether
  `editor->VSCode.TextEditor.document` succeeds or throws, the event's
  `textEditor`, `capturedEditor === state.editor`, `eventEditor ===
  state.editor`, `state.editor`'s `fileName`, and whether the registry still
  has `state.document`. A temporary test
  `test/tests/Test__Issue300.res` ran lifecycle A from the spec — load
  `InputMethod.agda` (capturing editor A in `Main.initialize`), open
  `Goals.agda` in the same editor group, switch back to `InputMethod.agda` via
  `showTextDocumentWithUri` (ordinary same-group reopen, no `Beside`/split
  groups), then provoke a selection change. A bare `cursorDown`/`cursorRight`
  and a `setSelection` on the reopened editor's handle produced **no**
  selection-change events at all (confirmed by an independent test-side
  `onDidChangeTextEditorSelection` listener logging nothing); only the VS Code
  `type` command (typing a character, which both edits the document and moves
  the cursor) reliably produced an observable selection-change event in this
  headless harness. With that probe:
  - `onOpenEditor #1 opened=.../InputMethod.agda
    prevStateEditorFileName=.../InputMethod.agda prevEditor===newEditor=no
    registryHasDoc=yes` — confirms ordinary same-group tab switching alone
    (no split editor groups) causes `Inputs.onOpenEditor` to fire for the
    reopened `InputMethod.agda` and replace `state.editor` with a new,
    different `TextEditor` reference (editor B), while the registry entry and
    its `state` stay live.
  - `selection: capturedAccess=ok:.../InputMethod.agda
    capturedEditor===state.editor=no eventEditor===state.editor=yes
    stateEditorFileName=.../InputMethod.agda registryHasStateDocument=yes` —
    confirms the very shape H6/H7 set out to find without resorting to split
    editor groups: the listener's captured `editor` (editor A, from
    `Main.initialize`) differs from the live `state.editor` (editor B, the
    replacement installed by `Inputs.onOpenEditor`), the incoming selection
    event's `textEditor` *is* the live `state.editor`, and the registry/state
    are both still alive. However, `editor->VSCode.TextEditor.document` on the
    stale captured editor A **succeeded** (`capturedAccess=ok:...`) — no
    throw, and no disposed-editor warning text appeared anywhere in the
    captured output.
- Interpretation: Ordinary same-group tab switching is sufficient — and
  strictly better than H7's split-editor-group harness — to reproduce the
  stale-capture shape: `Inputs.onOpenEditor` swaps in a fresh `TextEditor` for
  `state.editor` while the `onDidChangeTextEditorSelection` closure keeps
  referencing the original captured `editor`, and a real selection event
  arrives whose `textEditor` matches the new `state.editor` but not the
  captured one. This recreates, cleanly and without the H7 harness's
  collateral-invalidation problems, exactly the divergence the H6/H8
  stale-capture working model describes. That satisfies the "recreates the H6 stale-capture shape without
  using split editor groups" half of the **partially confirmed** criterion.
  But the other half of `confirmed` — the captured editor's document access
  throwing or producing the disposed-editor warning — did not happen: VS
  Code's `showTextDocumentWithUri` reopen of the same `TextDocument` in the
  same group apparently reuses/keeps-alive the underlying document and does
  not dispose editor A's `TextEditor` handle (or at least not synchronously
  enough, or not at all, for `VSCode.TextEditor.document(editor)` to observe
  it as disposed) by the time the next selection event fires. So the
  reachable-shape half is **confirmed**, the throw/warning half is **not
  observed**, landing this squarely in **partially confirmed** per the spec's
  classification table — closing most, but not all, of the link H6/H7 left
  open: a later experiment would still need to find a lifecycle (or a longer
  delay / more tab churn / lifecycle B or C from this same spec) under which
  `editor` (editor A) actually becomes `_disposed` before a selection event
  fires, in order to observe the live warning/throw this issue is ultimately
  about.
- Cleanup: removed the temporary `Js.log` instrumentation from
  `Inputs.onOpenEditor` and `onDidChangeTextEditorSelection` (and the
  `h8Counter` ref) in `src/Main/Main.res`, restoring it to its original form;
  deleted `test/tests/Test__Issue300.res`; restored `test/TestSuiteAdapter.res`
  from a backup (and removed the backup); rebuilt both the ReScript output
  (`npx rescript build`) and the webpack bundle (`npx webpack --mode
  development`) so the running extension matches the restored source; removed
  any stray `lib/bs/.../Test__Issue300*` compiled artifacts; removed scratch
  files in `/tmp`. Working tree left with only this documentation update.

### H9: delayed same-group disposal after tab churn

Hypothesis: H8 recreated the correct same-group stale-capture shape, but the
captured editor did not become disposed before the immediate selection probe.
The missing condition may be timing or tab churn: after `state.editor` is
replaced by ordinary same-group tab switching, VS Code may only mark the old
captured editor disposed after additional tab switches, closing/reopening the
same tab, or a short delay.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create one focused temporary diagnostic test, preferably
   `test/tests/Test__Issue300.res`, and minimal temporary instrumentation in
   `src/Main/Main.res`.
3. Reuse H8's same-group harness as the baseline. Do **not** use split editor
   groups as the primary lifecycle.
4. Add extension-side diagnostics in the same two places H8 used:
   - inside `Inputs.onOpenEditor` immediately before `state.editor = editor`;
   - at the start of `onDidChangeTextEditorSelection`, around the first
     `VSCode.TextEditor.document(editor)` access on the captured editor.
5. The logs must be extension-side and should record:
   - a variant label and monotonic step/counter;
   - opened/current document `fileName`;
   - whether `prevStateEditor === newEditor` in `Inputs.onOpenEditor`;
   - whether `capturedEditor === state.editor`;
   - whether `eventEditor === state.editor`;
   - whether `Registry.get(state.document)` is still present extension-side;
   - whether captured-editor document access succeeds or throws;
   - any exact `TextEditor is closed` / `TextEditor is disposed` warning text
     visible in extension-host output, if observable.
6. Establish the H8 baseline first:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load` so `Main.initialize` captures editor A;
   - open `test/tests/assets/Goals.agda` in the same editor group;
   - reopen/switch back to `InputMethod.agda` in the same group;
   - confirm `prevStateEditor === newEditor: no`,
     `capturedEditor !== state.editor`, `eventEditor === state.editor`, and
     registry/state still live.
7. Variant A — delayed probes:
   - after the H8 baseline replacement, wait 250ms, 1000ms, and 3000ms;
   - after each wait, trigger the most reliable H8 probe (`type` if plain
     cursor movement still does not emit selection events), then undo any edit;
   - record whether captured-editor access changes from `ok` to
     throw/warning over time.
8. Variant B — tab churn:
   - after the H8 baseline replacement, switch through several existing Agda
     fixture tabs in the same group, for example `Load.agda`, `Issue157.agda`,
     and `Goals.agda`;
   - return to `InputMethod.agda`;
   - repeat the selection/type probe and record whether `state.editor` was
     replaced again and whether captured-editor access now throws/warns.
9. Variant C — same-group close/reopen:
   - after establishing the H8 replacement, switch away to another file in the
     same group;
   - close the visible `InputMethod.agda` tab if it is still open, or close and
     reopen it using ordinary same-group commands/API only;
   - reopen `InputMethod.agda` in the same group;
   - repeat the selection/type probe and record whether captured-editor access
     now throws/warns.
10. Variant D — longer churn plus delay, only if A-C do not produce disposal:
    - repeat tab churn for multiple cycles;
    - wait 5000ms after returning to `InputMethod.agda`;
    - probe again.
11. For every variant, record raw observations:
    - exact lifecycle steps used;
    - whether same-group `state.editor` replacement occurred;
    - whether extension-side registry/state stayed live;
    - whether `capturedEditor !== state.editor` and
      `eventEditor === state.editor` held at probe time;
    - whether captured-editor document access succeeded, threw, or emitted the
      disposed-editor warning.
12. Interpret the result:
    - `confirmed`: a same-group tab-switch/churn/close-reopen lifecycle keeps
      state live, keeps `eventEditor === state.editor`, and makes captured
      editor access throw or emit the disposed-editor warning.
    - `falsified`: same-group replacement and repeated probes are reliable,
      but captured-editor access continues to succeed across delayed, churn,
      and close/reopen variants.
    - `inconclusive`: the harness cannot reliably emit events after the
      variants, cannot preserve live extension-side state, or cannot observe
      the warning/throw path.
13. Write the result, raw observations, and interpretation below in this file.
14. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, compiled artifacts, listeners, scratch files, and any test
    adapter changes. Leave the working tree with only the intended
    documentation update unless a permanent diagnostic was explicitly
    requested.

Result:

- Status: **falsified**.
- Observations: Temporary `Js.log` instrumentation (prefix `[issue#300 H9
  ext]`) was added at the same two points H8 used in `src/Main/Main.res`:
  inside `Inputs.onOpenEditor` immediately before `state.editor = editor`
  (logging a counter, the opened `fileName`, whether
  `prevStateEditor === newEditor`, and whether the registry still has live
  state), and at the start of `onDidChangeTextEditorSelection` (logging
  whether captured-editor document access succeeds/throws, whether
  `capturedEditor === state.editor`, whether `eventEditor === state.editor`,
  the current `state.editor` file name, and whether the registry still has
  live state). A temporary test `test/tests/Test__Issue300.res` ran the H8
  same-group baseline (load `InputMethod.agda`, open `Goals.agda`, switch
  back to `InputMethod.agda`) followed by Variant A (probes after 250ms,
  1000ms, and 3000ms delays), Variant B (tab churn through `Load.agda`,
  `Issue157.agda`, `Goals.agda`, back to `InputMethod.agda`), Variant C
  (switch away, close the active `InputMethod.agda` tab via
  `workbench.action.closeActiveEditor`, then reopen it), and Variant D
  (several more churn cycles followed by a 5000ms wait). Each probe used the
  H8-validated `type` command (which reliably produces a selection-change
  event in this headless harness) followed by `undo`. The single test passed
  in 13s with no uncaught exceptions.
  - Baseline: `onOpenEditor #1 opened=.../InputMethod.agda
    prevStateEditor===newEditor=no registryHasState=yes` — confirms the H8
    same-group replacement of `state.editor` reproduced cleanly.
  - Every single one of the ~10 selection-event probe lines across the
    baseline and Variants A, B, C, and D reads identically in shape:
    `selection capturedAccess=ok:.../InputMethod.agda
    capturedEditor===state.editor=no eventEditor===state.editor=yes
    stateEditorFileName=.../InputMethod.agda registryHasState=yes`.
  - `onOpenEditor` fired three further times with the expected
    `prevStateEditor===newEditor=no registryHasState=yes` shape — once when
    Variant B's churn returned focus to `InputMethod.agda` (`#2`), once when
    Variant C reopened the closed `InputMethod.agda` tab (`#3`), and once
    when Variant D's extra churn returned to it before the 5s wait (`#4`).
    `state.editor` was visibly replaced again at each of these points, yet
    the stale-capture shape and the live registry/state persisted unchanged.
  - No `TextEditor is closed` / `TextEditor is disposed` warning text and no
    thrown exception ever appeared, at any delay (up to 3000ms continuously,
    and again after a fresh 5000ms wait), through any amount of tab churn,
    or across a real same-group close-and-reopen of the tab.
- Interpretation: The same-group replacement lifecycle that H8 established is
  fully reproducible and stable — `Inputs.onOpenEditor` keeps swapping
  `state.editor` to a fresh `TextEditor` reference every time the
  `InputMethod.agda` tab regains focus (directly, after churn, or after a
  close/reopen), while `eventEditor === state.editor` and
  `capturedEditor !== state.editor` hold at every single probe, and the
  registry/state stay live throughout. But across delays up to 3 then 5
  seconds, multiple rounds of tab churn, and an explicit close/reopen of the
  same-group tab — i.e. every condition the hypothesis proposed as the
  "missing ingredient" — `editor->VSCode.TextEditor.document` on the captured
  (now-replaced) editor handle kept succeeding (`capturedAccess=ok:...`)
  with no throw and no disposed-editor warning. This is exactly the
  `falsified` shape from the spec's classification table: "same-group
  replacement and repeated probes are reliable, but captured-editor access
  continues to succeed across delayed, churn, and close/reopen variants."
  H9 therefore closes the disposal-timing question this branch of the
  H6/H8 working model left open: in this same-group, same-window harness,
  the previously captured `TextEditor` handle for `InputMethod.agda` simply
  never becomes "closed"/"disposed" from the extension's point of view, no
  matter how long you wait or how much you churn the tab — VS Code appears
  to keep recycling/retaining the underlying editor resource for a
  document that stays open in the same group, rather than disposing the
  handle the extension is still holding. Reproducing an actual
  throw/disposed-editor warning would likely require a lifecycle this
  same-group harness cannot produce (e.g. genuinely closing the document out
  from under the editor, not just the tab/group — which is the territory
  H7's split-group harness was reaching for, just via the wrong mechanism).
- Cleanup: removed the temporary `Js.log` instrumentation and the
  `h9Counter` ref from `src/Main/Main.res`; deleted
  `test/tests/Test__Issue300.res`; restored `test/TestSuiteAdapter.res` from
  a backup (`Glob.glob("**/*.js", ...)`); rebuilt both `rescript` and
  `webpack` to regenerate clean compiled output and bundle; removed stray
  compiled artifacts under `lib/bs/.../Test__Issue300*`; removed scratch
  files in `/tmp` (`issue300-*.log`, `TestSuiteAdapter.res.bak`).

### H10: genuine document-close / listener-teardown race

Hypothesis: same-group tab churn does not dispose the captured editor, but a
true document/editor close may. The #300 warning may occur in the narrow window
where VS Code has closed or disposed the captured `TextEditor`, but the
extension's per-state global listeners have not yet been removed by
`Registry.removeAndDestroy(document)`.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create one focused temporary diagnostic test, preferably
   `test/tests/Test__Issue300.res`, and minimal temporary extension-side
   instrumentation in `src/Main/Main.res` plus `src/Registry.res` only if
   needed for teardown logging.
3. Do **not** use direct test imports of `Registry` or `Singleton.Panel` as
   authoritative evidence. H5 proved those are separate module instances.
4. Add extension-side logs for ordering:
   - in `Main.initialize`, assign a diagnostic state/editor id for the
     captured editor;
   - at the start of `onDidChangeTextEditorSelection`;
   - at the start of `onDidChangeTextDocument`;
   - in the workspace document-close path immediately before/after
     `Registry.removeAndDestroy(document)`;
   - inside `Registry.removeAndDestroy`, if the previous log cannot prove
     teardown ordering.
5. Each listener log should record:
   - variant label and monotonic event counter;
   - captured editor id and captured document `fileName`, if accessible;
   - current `state.editor` `fileName`;
   - whether `capturedEditor === state.editor`;
   - whether event editor/document matches `state.editor` / `state.document`;
   - whether captured-editor document access succeeds or throws;
   - whether extension-side registry still has `state.document`;
   - any exact `TextEditor is closed` / `TextEditor is disposed` warning text
     visible in extension-host output, if observable.
6. Each teardown log should record:
   - `onDidCloseTextDocument` document `fileName`;
   - whether it matches `state.document.fileName`;
   - whether `Registry.removeAndDestroy` was called;
   - whether the state's subscriptions were disposed before or after any
     selection/text event log for the same document.
7. Establish a live state:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load`;
   - confirm extension-side registry/state is live by command result or
     extension-side log.
8. Variant A — close active editor and force document close:
   - run `workbench.action.closeActiveEditor`;
   - if `InputMethod.agda` remains in `VSCode.Workspace.textDocuments`, run
     stronger non-destructive close commands available in the harness, such as
     `workbench.action.closeAllEditors`;
   - record whether `onDidCloseTextDocument` fires for `InputMethod.agda`;
   - immediately after each close action, open or focus another file and
     trigger a selection/type probe.
9. Variant B — close all editors, then reopen another file first:
   - open `InputMethod.agda` and one or more other Agda fixtures;
   - execute `workbench.action.closeAllEditors`;
   - open a different file first, then reopen `InputMethod.agda`;
   - trigger selection/type probes and record whether any stale listener from
     the original `InputMethod.agda` state still fires.
10. Variant C — close document after dirty/edit state:
    - open `InputMethod.agda`;
    - make a reversible edit through the harness;
    - close using the least destructive command that does not require manual UI
      confirmation in the test environment;
    - undo/revert if needed to keep fixtures clean;
    - record whether dirty-state close changes `onDidCloseTextDocument`,
      listener teardown, or captured-editor disposal behavior.
11. Variant D — rapid close/probe race:
    - after `agda-mode.load`, start the close action and immediately queue a
      focus/selection/type action on another editor without long waits;
    - record event ordering from extension-side counters.
12. Interpret the result:
    - `confirmed`: a true document/editor close produces a selection/text event
      before listener teardown where captured-editor access throws or emits the
      disposed-editor warning.
    - `falsified`: the harness can trigger true `onDidCloseTextDocument` and
      teardown, but listeners are removed before any stale captured-editor
      access can occur, and no warning/throw appears.
    - `inconclusive`: the harness still cannot trigger true document close for
      `InputMethod.agda`, cannot observe extension-side teardown ordering, or
      cannot observe the warning/throw path.
13. Write the result, raw observations, and interpretation below in this file.
14. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, compiled artifacts, listeners, scratch files, and any test
    adapter changes. Leave the working tree with only the intended
    documentation update unless a permanent diagnostic was explicitly
    requested.

Result:

- Status: **inconclusive**.
- Raw observations: Instrumented `Main.initialize` (selection-change,
  text-change handlers, and `onCloseDocument`) and `State.destroy` (before/after
  disposing subscriptions), then ran four variants against a live
  `InputMethod.agda` state:
  - Variant A (close active editor + immediately probe elsewhere): the
    `InputMethod.agda` state kept emitting `textChange`/`selection` log lines
    with `capturedAccess=ok:...`, `capturedEditor===state.editor=yes`, and
    `registryHasState=yes` throughout — i.e. the state was never destroyed by
    `workbench.action.closeActiveEditor`.
  - Variant B (close all editors, reopen elsewhere first, reopen
    `InputMethod.agda`): exactly one `State.destroy: before/after disposing
    subscriptions` pair was logged for `InputMethod.agda`, back-to-back with no
    event in between — but this fired while reloading via
    `AgdaMode.makeAndLoad`, i.e. through registry replacement, not through a
    genuine `onDidCloseTextDocument`.
  - Variants C and D (dirty-edit-then-close, and rapid close/probe race):
    same pattern — `textChange`/`selection` events kept firing for
    `InputMethod.agda` with `capturedAccess=ok:...`, never a throw or warning.
  - **`[issue#300 H10 ext] onCloseDocument` never appeared once** across the
    entire run — `Inputs.onCloseDocument` was never invoked for
    `InputMethod.agda`, meaning `Registry.removeAndDestroy` was never reached
    via a genuine document-close path in this harness.
- Interpretation: the harness cannot trigger a true `onDidCloseTextDocument`
  for `InputMethod.agda` — closing the active editor, closing all editors, and
  reverting+closing a dirty editor all leave the underlying `TextDocument` open
  (and the extension's state alive and responsive) in this headless VS Code
  test environment. The single observed `State.destroy` happened through state
  replacement on reload, not document close, and showed clean, synchronous,
  back-to-back disposal with no intervening event — informative about ordering
  in that path, but not about the genuine-close race the hypothesis targets.
  Per the spec's classification table this is **inconclusive**: "the harness
  still cannot trigger true document close for `InputMethod.agda` ... or cannot
  observe the warning/throw path." Reproducing #300 likely requires either a
  way to force VS Code to truly close/dispose the backing `TextDocument` in the
  test harness, or accepting that this race can only be observed in a real
  multi-window/multi-editor user session rather than the single-window headless
  harness used here.
- Cleanup: removed `h10Counter` and all three `[issue#300 H10 ext]` log blocks
  from `src/Main/Main.res`, removed the before/after disposing-subscriptions
  logs from `src/State/State.res`, deleted `test/tests/Test__Issue300.res`,
  restored `test/TestSuiteAdapter.res`, rebuilt `rescript`+`webpack`, and
  removed stray compiled artifacts and `/tmp` scratch files.

### H11: automated stale captured-editor route detection

Hypothesis: the headless harness cannot deterministically reproduce the exact
"TextEditor is closed/disposed" warning, but it can deterministically reproduce
a candidate precursor route plausibly connected to that warning: after H8's same-group
editor replacement, `onDidChangeTextEditorSelection` / `onDidChangeTextDocument`
still access the stale captured editor on `master`.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus minimal temporary extension-side
   instrumentation in `src/Main/Main.res`.
3. Do **not** use direct test imports of `Registry`, `Singleton.Panel`, or
   other singleton modules as authoritative evidence. H5 proved those can
   observe the wrong module instance.
4. Add temporary extension-side counters/logs for stale captured-editor access:
   - in `Inputs.onOpenEditor`, record when `state.editor` is replaced and
     whether `prevStateEditor === newEditor`;
   - at the start of `onDidChangeTextEditorSelection`, record whether the
     handler accesses the captured `editor` and whether
     `capturedEditor === state.editor`;
   - at the start of `onDidChangeTextDocument`, record whether
     `IM.Input.fromTextDocumentChangeEvent`, `Tokens.applyEdit`, or
     `Goals.scanAllGoals` would be called with the captured `editor` while
     `capturedEditor !== state.editor`;
   - expose the recorded facts through a temporary extension-side command such
     as `agda-mode.issue300-dump-routing`, or through deterministic log lines
     consumed by the test.
5. The diagnostic should record at minimum:
   - `stateEditorReplacementCount`;
   - `selectionCapturedAccessCount`;
   - `textChangeCapturedAccessCount`;
   - `capturedEditor !== state.editor` at the time of each access;
   - `eventEditor === state.editor` for selection events;
   - `event.document.fileName === state.document.fileName` for text changes;
   - whether extension-side registry/state stayed live.
6. Run the deterministic H8 lifecycle on unmodified `master` behavior:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load`;
   - open `test/tests/assets/Goals.agda` in the same group;
   - switch back to `InputMethod.agda`;
   - trigger the H8-validated `type` probe, then undo.
7. Record the master-side expected failure signal:
   - `stateEditorReplacementCount >= 1`;
   - `selectionCapturedAccessCount > 0` and/or
     `textChangeCapturedAccessCount > 0`;
   - at least one captured access happened while
     `capturedEditor !== state.editor`;
   - the event belonged to the current `state.editor` / `state.document`.
8. Apply the minimal #304-style routing change temporarily, scoped only to the
   experiment:
   - selection events must route through `state.editor` / `state.document`, not
     the captured `editor`;
   - text-change handling must ignore events whose document does not match
     `state.document`;
   - relevant calls should use `state.editor` where appropriate.
9. Rebuild and rerun the exact same H8 lifecycle and probe with the same
   diagnostic counters.
10. Record the fixed-side expected pass signal:
    - `stateEditorReplacementCount >= 1`;
    - no stale captured-editor access occurs while
      `capturedEditor !== state.editor`;
    - events still route to the live `state.editor` / `state.document`;
    - no regression in the observed IM selection/text path for
      `InputMethod.agda`.
11. Interpret the result:
    - `confirmed`: unmodified master deterministically hits stale captured
      editor access after H8 replacement, and the #304-style routing change
      removes that stale access for the exact same automated lifecycle.
    - `falsified`: unmodified master does not hit stale captured-editor access
      in the automated H8 lifecycle, or the #304-style routing change does not
      remove it.
    - `inconclusive`: the harness cannot emit deterministic selection/text
      events, cannot observe extension-side counters reliably, or the temporary
      #304-style patch cannot be applied cleanly.
12. Write the result, raw observations, and interpretation below in this file.
13. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, compiled artifacts, scratch files, and the temporary
    #304-style patch. Leave the working tree with only the intended
    documentation update unless a permanent regression test/fix was explicitly
    requested.

Result:

- Status: **inconclusive** (the chosen metric cannot test the hypothesis —
  see Interpretation).
- Observations: Built `issue#300-new` (without the #304 routing fix) with
  temporary instrumentation: a module `H11Routing` in `src/Main/Main.res`
  holding mutable counters (`stateEditorReplacementCount`,
  `selectionCapturedAccessCount`, `textChangeCapturedAccessCount`,
  `staleSelectionAccessCount`/`staleTextChangeAccessCount` for accesses where
  `capturedEditor !== state.editor`, `selectionEventMatchesStateCount`,
  `textChangeDocMatchesStateCount`, `registryStayedLive`), incremented inside
  `Inputs.onOpenEditor` (replacement count), `onDidChangeTextEditorSelection`,
  and `onDidChangeTextDocument`, exposed via a temporary command
  `agda-mode.issue300-dump-routing` returning a tuple snapshot. A temporary
  test `test/tests/Test__Issue300.res` ran the H8-validated deterministic
  lifecycle — load `InputMethod.agda`, open `Goals.agda` in the same group,
  switch back to `InputMethod.agda` via `File.open_`/`showTextDocumentWithUri`,
  fire the H8 `type` probe followed by `undo` — then dumped the counters.
  - **Master-side run** (`/tmp/issue300-h11-master.log`):
    `stateEditorReplacementCount=1 selectionCapturedAccessCount=2
    textChangeCapturedAccessCount=4 staleSelectionAccessCount=2
    staleTextChangeAccessCount=4 selectionEventMatchesStateCount=2
    textChangeDocMatchesStateCount=4 registryStayedLive=yes`. This matches the
    spec's predicted master-side failure signal exactly:
    `stateEditorReplacementCount >= 1`, captured-access counts `> 0`, **every**
    captured access happened while `capturedEditor !== state.editor`
    (`stale*Count == *CapturedAccessCount`), and the incoming events still
    belonged to the live `state.editor`/`state.document`
    (`*MatchesStateCount == *CapturedAccessCount`).
  - Applied a minimal temporary #304-style routing patch scoped to the same
    two listeners: the selection handler now derives `document` from
    `state.document` instead of `VSCode.TextEditor.document(editor)`, and the
    text-change handler now early-guards on
    `event.document.fileName == state.document.fileName` and routes
    `IM.Input.fromTextDocumentChangeEvent`, `Tokens.applyEdit`, and
    `Goals.scanAllGoals` through `state.editor` instead of the captured
    `editor`. Rebuilt (`rescript` + `webpack --mode development`) and reran the
    identical lifecycle and probe.
  - **Patched-side run** (`/tmp/issue300-h11-patched.log`): **identical**
    counters — `stateEditorReplacementCount=1 selectionCapturedAccessCount=2
    textChangeCapturedAccessCount=4 staleSelectionAccessCount=2
    staleTextChangeAccessCount=4 selectionEventMatchesStateCount=2
    textChangeDocMatchesStateCount=4 registryStayedLive=yes`.
- Interpretation: The byte-identical before/after counts do **not** show that
  a #304-style routing change "does not remove stale captured-editor access,"
  because the counters never measured what the spec needed them to measure.
  `selectionCapturedAccessCount`/`textChangeCapturedAccessCount` and their
  `stale*` companions were incremented by instrumentation placed at the *top*
  of `onDidChangeTextEditorSelection`/`onDidChangeTextDocument` — i.e. they
  count "the listener closure fired while `capturedEditor !== state.editor`,"
  a fact about *which closure VS Code invoked*, not about whether that
  closure's body went on to *dereference or consume* the captured `editor`
  (`VSCode.TextEditor.document(editor)`, or passing `editor` into
  `IM.Input.fromTextDocumentChangeEvent`/`Tokens.applyEdit`/
  `Goals.scanAllGoals`). Both the master body and the patched body run inside
  the same closure over the same captured `editor`, so naturally both fire the
  same number of times on the same events — the patch changes *what the body
  does after entry*, which this metric cannot see. That makes the patch
  comparison **inconclusive**: it neither confirms nor falsifies whether
  #304-style routing eliminates stale-editor *use*, because the instrument is
  overbroad/invalid for that question — it would have produced byte-identical
  numbers even if the patched body fully stopped touching the captured
  `editor`. A metric that could actually decide this would need separate
  counters for "closure fired while stale" vs. "closure body
  dereferenced/passed the captured `editor`," so the before/after delta in the
  *latter* could be compared (master: nonzero; #304-style patch: should drop
  to zero, if the hypothesis holds). What H11 *does* establish reliably is the
  master-side baseline (carried over from H8): the global listeners are
  long-lived closures over the originally-captured `editor`, and they keep
  firing — on live, matching events — while `capturedEditor !== state.editor`.
  Whether a #304-style routing fix actually stops those closures from
  dereferencing that stale reference remains untested.
- Cleanup: removed the temporary `H11Routing` module, the three
  instrumentation blocks (`Inputs.onOpenEditor`, the two listener closures),
  the temporary `agda-mode.issue300-dump-routing` command registration, and
  the temporary #304-style routing patch from `src/Main/Main.res`, restoring
  it to its original form; deleted `test/tests/Test__Issue300.res`; restored
  `test/TestSuiteAdapter.res` from a backup (and removed the backup); rebuilt
  both the ReScript output (`npx rescript build`) and the webpack bundle
  (`npx webpack --mode development`) so the running extension matches the
  restored source; removed compiled `Test__Issue300*` artifacts and scratch
  files in `/tmp`. Working tree left with only this documentation update.

### H12: deterministic stale captured-editor use measurement

Hypothesis: H11's closure-firing metric was too broad, but the same automated
H8 lifecycle can measure actual stale captured-editor *use* directly:
`VSCode.TextEditor.document(editor)` dereferences and calls that pass the
captured `editor` into IM/tokens/goals. On master those stale-use counters
should be nonzero after editor replacement.

Lab assistant task:

1. Work on `issue#300-new` without applying the #304 routing fix.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus minimal temporary extension-side
   instrumentation in `src/Main/Main.res`.
3. Do **not** use direct test imports of `Registry`, `Singleton.Panel`, or
   other singleton modules as authoritative evidence. H5 proved those can
   observe the wrong module instance.
4. Add temporary extension-side counters that distinguish closure firing from
   captured-editor use:
   - `stateEditorReplacementCount`;
   - `selectionClosureWhileStaleCount`;
   - `selectionCapturedDocumentDereferenceCount`;
   - `selectionStateDocumentUseCount`;
   - `textChangeClosureWhileStaleCount`;
   - `textChangeCapturedEditorPassedToIMCount`;
   - `textChangeCapturedEditorPassedToTokensCount`;
   - `textChangeCapturedEditorPassedToGoalsCount`;
   - `textChangeStateEditorUseCount`;
   - `eventMatchesStateCount`;
   - `registryStayedLive`.
5. Increment the stale-use counters only immediately before the real operation
   being measured:
   - before `VSCode.TextEditor.document(editor)` in the selection handler;
   - before `IM.Input.fromTextDocumentChangeEvent(editor, event)`;
   - before `Tokens.applyEdit(editor, event)`;
   - before `Goals.scanAllGoals(editor, changes)`.
6. In the temporary #304-style patch, keep the closure-firing counters, but
   increment separate state-use counters immediately before using
   `state.document` or `state.editor`. The stale-use counters must only count
   captured `editor` use, not listener entry.
7. Expose the counters through a temporary extension-side command such as
   `agda-mode.issue300-dump-routing-use`, or deterministic extension-side logs
   consumed by the test.
8. Run the deterministic H8 lifecycle on unmodified `master` behavior:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load`;
   - open `test/tests/assets/Goals.agda` in the same editor group;
   - switch back to `InputMethod.agda`;
   - trigger the H8-validated `type` probe, then undo.
9. Record the master-side expected failure signal:
   - `stateEditorReplacementCount >= 1`;
   - at least one closure fired while `capturedEditor !== state.editor`;
   - at least one stale-use counter is nonzero;
   - the event belonged to the current `state.editor` / `state.document`;
   - extension-side registry/state stayed live.
10. Apply the minimal #304-style routing change temporarily, scoped only to the
    experiment:
    - selection should use `state.document` / `state.editor`, not
      `VSCode.TextEditor.document(editor)`;
    - text-change handling should ignore nonmatching documents;
    - matching text-change handling should route IM/tokens/goals through
      `state.editor` and `state.document`, not the captured `editor`.
11. Rebuild and rerun the exact same H8 lifecycle and probe with the same
    corrected counters.
12. Record the patched-side expected pass signal:
    - `stateEditorReplacementCount >= 1`;
    - closure-while-stale counters may remain nonzero;
    - all stale captured-editor use counters are zero;
    - state-use counters are nonzero for the matching `InputMethod.agda`
      events;
    - no regression in the observed IM selection/text path for
      `InputMethod.agda`.
13. Interpret the result:
    - `confirmed`: master has nonzero stale captured-editor use after H8
      replacement, and the #304-style routing patch drives all stale-use
      counters to zero while preserving state-editor/state-document handling.
    - `falsified`: master has no stale captured-editor use in the automated H8
      lifecycle, or the #304-style patch still uses the captured `editor`.
    - `inconclusive`: the harness cannot emit deterministic selection/text
      events, cannot observe extension-side counters reliably, or the temporary
      #304-style patch cannot be applied cleanly.
14. Write the result, raw observations, and interpretation below in this file.
15. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, compiled artifacts, scratch files, and the temporary
    #304-style patch. Leave the working tree with only the intended
    documentation update unless a permanent regression test/fix was explicitly
    requested.

Result:

- Status: **confirmed**.
- Observations (H8-validated lifecycle: load `InputMethod.agda`, open
  `Goals.agda` in the same group, switch back to `InputMethod.agda`, then run
  the `type`/`undo` probe):
  - Master (captured-`editor` baseline, 1 passing):
    `stateEditorReplacementCount=1 selectionClosureWhileStaleCount=2
    selectionCapturedDocumentDereferenceCount=2 selectionStateDocumentUseCount=0
    textChangeClosureWhileStaleCount=4 textChangeCapturedEditorPassedToIMCount=4
    textChangeCapturedEditorPassedToTokensCount=4
    textChangeCapturedEditorPassedToGoalsCount=2 textChangeStateEditorUseCount=0
    eventMatchesStateCount=6 registryStayedLive=yes`
  - Patched (#304-style routing through `state.editor`/`state.document`,
    1 passing):
    `stateEditorReplacementCount=1 selectionClosureWhileStaleCount=2
    selectionCapturedDocumentDereferenceCount=0 selectionStateDocumentUseCount=2
    textChangeClosureWhileStaleCount=4 textChangeCapturedEditorPassedToIMCount=0
    textChangeCapturedEditorPassedToTokensCount=0
    textChangeCapturedEditorPassedToGoalsCount=0 textChangeStateEditorUseCount=4
    eventMatchesStateCount=6 registryStayedLive=yes`
- Interpretation: unlike H11's closure-entry counters (which are necessarily
  identical across master/patched bodies because both run inside the same
  long-lived closure over the same captured `editor`), H12's counters sit
  immediately before the actual operation being measured — the `document`
  derivation in the selection handler, and the `editor` argument passed into
  `IM.Input.fromTextDocumentChangeEvent`/`Tokens.applyEdit`/`Goals.scanAllGoals`
  in the text-change handler — paired with mutually exclusive "routed through
  `state`" counters. On master, every one of those operations dereferences/uses
  the stale captured `editor` (`selectionCapturedDocumentDereferenceCount=2`,
  `textChangeCapturedEditorPassedToIMCount=4`,
  `textChangeCapturedEditorPassedToTokensCount=4`,
  `textChangeCapturedEditorPassedToGoalsCount=2`) while the `state`-routed
  counters stay at zero, even though the listener closures keep firing on live,
  matching events (`selectionClosureWhileStaleCount=2`,
  `textChangeClosureWhileStaleCount=4`, `eventMatchesStateCount=6`,
  `registryStayedLive=yes`). With the minimal #304-style patch — guard the
  text-change handler on `event.document.fileName == state.document.fileName`
  and route the `document`/`editor` arguments through `state.document`/
  `state.editor` instead of the captured `editor` — every stale-use counter
  drops to zero and the corresponding state-routed counters
  (`selectionStateDocumentUseCount=2`, `textChangeStateEditorUseCount=4`) pick
  up exactly the same totals the stale-use counters had on master, with
  `eventMatchesStateCount` and `registryStayedLive` unchanged. H12 confirmed
  deterministic stale captured-editor *use* on master (not just stale closure
  firing): the global `onDidChangeTextEditorSelection`/`onDidChangeTextDocument`
  listeners dereference/pass the stale captured `editor` after a same-group
  editor replacement. H12 did not reproduce the disposed-editor warning itself.
- Cleanup: `H12Use` module, all temporary instrumentation/patch blocks and
  dump-command registrations were removed from `src/Main/Main.res`,
  `test/tests/Test__Issue300.res` was deleted, `test/TestSuiteAdapter.res` was
  restored from backup, rescript/webpack were rebuilt, compiled
  `Test__Issue300*` artifacts were removed from `lib/js`/`lib/bs`, and all
  scratch log files were deleted.

### H13: automated disposed-TextEditor lifecycle discovery

Hypothesis: the missing ingredient for reproducing #300 is not stale captured
editor use — H12 already proved that — but a deterministic VS Code lifecycle
that makes a captured `TextEditor` handle actually become closed/disposed in
the headless integration harness. If such a lifecycle exists for any editor
handle, it should be adaptable to the captured Agda editor and should produce
the actual disposed-editor throw or warning.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus minimal temporary extension-side
   instrumentation only if needed to observe the Agda listener path.
3. Phase 1: generic disposed-editor discovery. Build a matrix of editor
   lifecycles and test whether a captured `TextEditor` handle becomes
   unusable:
   - workspace file opened normally with `showTextDocument`;
   - workspace file opened as preview, if the API/command allows it;
   - untitled document;
   - temporary file outside the workspace under `/tmp`;
   - same document in one editor group;
   - same document in two editor groups, if still worth checking as a control.
4. For each captured editor handle, run a bounded set of non-destructive close
   actions, recording the exact action used:
   - `workbench.action.closeActiveEditor`;
   - `workbench.action.closeAllEditors`;
   - `workbench.action.closeEditorsInGroup`;
   - `workbench.action.revertAndCloseActiveEditor`, only for temporary/dirty
     scratch documents where it cannot damage fixtures;
   - any existing VS Code API exposed by the bindings that closes/hides a
     specific editor or document.
5. After each close action, immediately and after short waits
   (100ms, 500ms, 1000ms), probe the captured handle:
   - call `VSCode.TextEditor.document(editor)` inside a catch/probe wrapper;
   - if document access succeeds, record the returned `fileName`;
   - if it throws, record the exact exception message;
   - capture extension-host output and search for `TextEditor is closed` /
     `TextEditor is disposed`;
   - record whether `onDidCloseTextDocument` fired for the document.
6. Interpret Phase 1:
   - if no matrix entry makes any captured `TextEditor` handle throw or emit
     the disposed-editor warning, stop and record H13 as `inconclusive`: the
     harness still cannot create the required disposed editor handle.
   - if one or more entries do produce a disposed handle, continue to Phase 2
     using the smallest deterministic lifecycle.
7. Phase 2: apply the discovered lifecycle to the Agda listener route:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load` so `Main.initialize` captures editor A;
   - create the H8 same-group replacement so `state.editor` moves away from
     editor A while the listener closure still captures editor A;
   - apply the Phase 1 disposal lifecycle to editor A as closely as possible;
   - trigger the H8-validated `type` probe or the most reliable selection/text
     event on the current Agda editor.
8. Add extension-side logs/counters around the listener operation that should
   fail:
   - before and after `VSCode.TextEditor.document(editor)` in the selection
     handler;
   - before passing captured `editor` to IM/tokens/goals in the text-change
     handler;
   - whether `capturedEditor !== state.editor`;
   - whether the event belongs to `state.editor` / `state.document`;
   - whether registry/state stayed live.
9. Record raw observations:
   - exact Phase 1 lifecycle that produced a disposed handle, if any;
   - exact exception/warning text;
   - whether the same lifecycle can be applied to the captured Agda editor;
   - whether the Agda listener path throws or emits the disposed-editor warning.
10. Interpret the result:
    - `confirmed`: an automated lifecycle makes the captured Agda editor
      closed/disposed, and a subsequent listener event touches it and throws or
      emits the disposed-editor warning.
    - `falsified`: a generic disposed handle exists, but applying the same
      lifecycle to the captured Agda editor never makes the listener throw or
      warn.
    - `inconclusive`: the harness cannot create any disposed `TextEditor`
      handle, cannot adapt the lifecycle to the Agda captured editor, or cannot
      observe the throw/warning path reliably.
11. Write the result, raw observations, and interpretation below in this file.
12. Clean up the lab: remove temporary tests, instrumentation, scratch files,
    generated temp files, compiled artifacts, and test adapter changes. Leave
    the working tree with only the intended documentation update unless a
    permanent automated reproducer was explicitly requested.

Result:

- Status: **inconclusive** (Phase 1 found no disposed-handle lifecycle, so
  Phase 2 was not attempted, per the interpretation rules above).
- Observations — Phase 1 matrix (each cell probed immediately, then at
  +100ms/+500ms/+1000ms after the close action; all 14 combinations returned
  `ok:<fileName>` at every offset, i.e. the captured `TextEditor` handle's
  `.document` and `.document->fileName` kept working normally):
  - workspace file, opened normally — `closeActiveEditor`, `closeAllEditors`,
    `closeEditorsInGroup`: all `ok`, handle still resolves to
    `InputMethod.agda` after the close action.
  - workspace file, opened as preview (`TextDocumentShowOptions.preview =
    true`) — `closeActiveEditor`, `closeAllEditors`: all `ok`.
  - untitled document (`vscode.workspace.openTextDocument` with inline
    content) — `closeActiveEditor`, `closeAllEditors`,
    `revertAndCloseActiveEditor`: all `ok:Untitled-1`.
  - temporary file under `/tmp` outside the workspace —
    `closeActiveEditor`, `closeAllEditors`, `revertAndCloseActiveEditor`:
    all `ok:/var/.../issue300-h13-scratch.agda`.
  - same document replaced within the same editor group (the H8 lifecycle,
    as a control) — `closeAllEditors`: `ok`, consistent with H8-H12's finding
    that the captured handle stays alive and merely becomes stale.
  - same document opened in two editor groups (`splitEditor`, as a control)
    — `closeEditorsInGroup`, `closeAllEditors`: all `ok`.
  - No "TextEditor is closed" / "TextEditor is disposed" text appeared
    anywhere in the captured extension-host output for any combination, and
    the single test (`1 passing`) completed without throwing.
- Interpretation: none of the six lifecycles crossed with their close actions
  — ordinary workspace file, preview tab, untitled document, external `/tmp`
  file, same-document same-group replacement, or same-document split into two
  groups — made a captured `TextEditor` handle become unusable in the headless
  integration harness. `VSCode.TextEditor.document(editor)` and the
  document's `fileName` kept resolving correctly up to a full second after
  every non-destructive close action tried (`closeActiveEditor`,
  `closeAllEditors`, `closeEditorsInGroup`, `revertAndCloseActiveEditor`).
  This matches the pattern already seen across H8-H12: in this harness, a
  captured `TextEditor`/`editor` reference outlives tab switches and editor
  closures as a *live but stale* handle rather than a *disposed* one — the
  VS Code proxy that throws "TextEditor is closed"/"is disposed" in the real
  desktop application apparently does not get exercised by any of the
  close/replace primitives available to this headless test harness. Per the
  task's own interpretation rule ("if no matrix entry makes any captured
  `TextEditor` handle throw or emit the disposed-editor warning, stop and
  record H13 as `inconclusive`: the harness still cannot create the required
  disposed editor handle"), Phase 1 is recorded as inconclusive and Phase 2
  (adapting the lifecycle to the captured Agda editor) was not attempted —
  there is no discovered lifecycle to adapt. For this reproduction track, the
  conclusion is: the harness cannot automatically reproduce #300 because none
  of the tested lifecycles produce a disposed editor handle.
- Cleanup: the temporary `Test__Issue300.res` integration test, the
  `TestSuiteAdapter.res` glob override, the scratch `/tmp` Agda file, and all
  scratch log files were removed; `src/Main/Main.res` required no temporary
  instrumentation for this experiment (Phase 2 was never reached) and remains
  untouched.

### H14: operation-specific TextEditor disposal probe

Hypothesis: H13 did not reproduce #300 because it only probed
`TextEditor.document` / `TextDocument.fileName`. In the headless integration
harness, a stale or closed `TextEditor` handle may continue to expose its
document while still throwing or emitting "TextEditor is closed/disposed" for
editor operations that touch the UI/editor proxy, such as reading or mutating
selection, revealing a range, applying decorations, or running an edit.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation only if the Agda captured-editor route cannot be observed
   from the test side. Do not use any manual GUI steps.
3. Build a reusable probe helper that takes a label and a captured
   `VSCode.TextEditor.t`, runs each operation below inside a catch/try wrapper,
   and records `ok:<detail>` or `throw:<exact message>`:
   - `VSCode.TextEditor.document(editor)` and `document.fileName` as the H13
     control;
   - read `editor.selection`;
   - read `editor.selections`;
   - set `editor.selection` to a zero-width selection at the document start;
   - set `editor.selections` to a single zero-width selection at the document
     start;
   - `editor.revealRange` on the first character or empty start range;
   - `editor.setDecorations` with a temporary decoration type and an empty
     range array;
   - `editor.edit` with a no-op callback, if the available bindings can do this
     without changing fixture text;
   - any other `TextEditor` operation already used by
     `src/Editor.res`, `src/Tokens.res`, `src/Goals.res`, or
     `src/State/State__Command.res` that can be exercised safely.
4. Capture extension-host output for the entire test and search it for exact
   warning text including `TextEditor is closed`, `TextEditor is disposed`,
   `closed`, and `disposed`. Record both thrown exceptions and logged warnings;
   the reported #300 symptom may be a warning without a thrown test exception.
5. Phase 1: run the operation probe against generic captured editor handles
   after the same lifecycle/close-action classes H13 tried:
   - workspace file opened normally, then `closeActiveEditor`,
     `closeAllEditors`, and `closeEditorsInGroup`;
   - workspace file opened as preview, then `closeActiveEditor` and
     `closeAllEditors`;
   - untitled document, then `closeActiveEditor`, `closeAllEditors`, and
     `revertAndCloseActiveEditor`;
   - temporary file under `/tmp`, then `closeActiveEditor`, `closeAllEditors`,
     and `revertAndCloseActiveEditor`;
   - H8 same-group replacement control, then `closeAllEditors`;
   - same document split into two groups, then `closeEditorsInGroup` and
     `closeAllEditors`.
6. For each Phase 1 row, probe at the same offsets H13 used: immediately,
   +100ms, +500ms, and +1000ms after the close/replacement action. Record the
   exact lifecycle, operation, delay, return/throw result, and any extension-host
   warning text.
7. Phase 2: regardless of whether Phase 1 finds a generic failure, run the same
   operation probe on the Agda captured-editor route that H12 proved stale:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load`;
   - capture the original editor A from the extension side if test-side identity
     is not authoritative;
   - open `test/tests/assets/Goals.agda` in the same editor group;
   - switch back to `InputMethod.agda` so `state.editor` moves to the current
     editor while the original listener closure still holds editor A;
   - confirm the stale shape with extension-side evidence:
     `capturedEditor !== state.editor` and the event/document still matches
     `state.editor` / `state.document`;
   - run the operation probe on captured editor A at the same delay offsets.
8. Phase 3: trigger the real listener path after the Phase 2 stale shape:
   - fire the H8/H12 `type` probe, then undo to keep the fixture clean;
   - capture whether the selection listener or text-change listener emits the
     exact disposed-editor warning when it performs its normal work;
   - record the same H12 stale-use counters if instrumentation is already
     present, but do not treat stale-use counters alone as reproduction.
9. Interpret the result:
   - `confirmed`: an automated operation on the stale captured Agda editor, or
     the real Agda listener path after the H8/H12 stale setup, throws or logs
     the exact "TextEditor is closed/disposed" warning.
   - `partially confirmed`: a generic captured editor handle throws/logs the
     exact warning for an editor operation, but the same operation cannot yet be
     adapted to the stale captured Agda editor or listener route.
   - `falsified`: all tested operations on generic handles and the stale Agda
     captured editor remain usable, and the real listener path produces no
     disposed-editor warning.
   - `inconclusive`: the helper cannot safely exercise the needed operations,
     extension-host warnings cannot be captured reliably, or the Agda stale
     captured-editor identity cannot be observed with extension-side evidence.
10. Write the result, raw observation table, exact warning/exception text, and
    interpretation below in this file.
11. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, temporary decoration types, scratch files, compiled
    artifacts, and any `/tmp` files. Leave the working tree with only the
    intended documentation update unless a permanent automated reproducer was
    explicitly requested.

Result:

**Confirmed** (operation-specific closed-editor throw on the stale Agda
captured-editor route) — `edit` called on the stale Agda captured-editor
(`Phase-2`) threw `"TextEditor#edit not possible on closed editors"`,
satisfying the "automated operation throws on the stale captured Agda editor"
condition in the interpretation rules. All other probed operations silently
succeed. The Agda global listeners (`onDidChangeTextDocument`,
`onDidChangeTextEditorSelection`) use `setDecorations`/`revealRange`, not
`TextEditor.edit`, so the #300 listener path itself does not reach the
throwing code path and the original disposed-editor warning remains
unreproduced through the listener route.

Raw observations (all 14 Phase-1 lifecycle × close-action combos, 4 delay
offsets each; Phase-2 Agda stale-editor probe; Phase-3 listener trigger):

| Combo | `document.fileName` | `selection.read/set` | `selections.read/set` | `revealRange` | `setDecorations` | `edit` |
|-------|---------------------|----------------------|-----------------------|---------------|------------------|--------|
| workspace-file-normal / closeActiveEditor | ok | ok | ok | ok | ok | **throw** |
| workspace-file-normal / closeAllEditors | ok | ok | ok | ok | ok | **throw** |
| workspace-file-normal / closeEditorsInGroup | ok | ok | ok | ok | ok | **throw** |
| workspace-file-preview / closeActiveEditor | ok | ok | ok | ok | ok | **throw** |
| workspace-file-preview / closeAllEditors | ok | ok | ok | ok | ok | **throw** |
| untitled-document / closeActiveEditor | ok | ok | ok | ok | ok | **throw** |
| untitled-document / closeAllEditors | ok | ok | ok | ok | ok | **throw** |
| untitled-document / revertAndCloseActiveEditor | ok | ok | ok | ok | ok | **throw** |
| tmp-file-outside-workspace / closeActiveEditor | ok | ok | ok | ok | ok | **throw** |
| tmp-file-outside-workspace / closeAllEditors | ok | ok | ok | ok | ok | **throw** |
| tmp-file-outside-workspace / revertAndCloseActiveEditor | ok | ok | ok | ok | ok | **throw** |
| same-document-same-group / closeAllEditors | ok | ok | ok | ok | ok | **throw** |
| same-document-two-groups / closeEditorsInGroup | ok | ok | ok | ok | ok | ok (doc still open in other group) |
| same-document-two-groups / closeAllEditors | ok | ok | ok | ok | ok | **throw** |
| Phase-2: stale Agda captured-editor | ok | ok | ok | ok | ok | **throw** |
| Phase-3: `VSCode.TextEditor.edit` (no-op builder) on Goals.agda — intended to fire `onDidChangeTextDocument` on the active editor while stale captured-editor is in scope | — | — | — | — | — | `edit=ok:true`; no disposed-editor warning in extension-host output; whether `onDidChangeTextDocument` listener actually fired or incremented stale-use counters was not verified |

Exact exception text when `edit` throws:
```
TextEditor#edit not possible on closed editors
```

Delay had no effect: results at `immediate`, `+100ms`, `+500ms`, `+1000ms`
were identical for every combo.

Interpretation:

- `edit` is the single operation that distinguishes a closed handle. All six
  other probed operations — `selection.read`, `selection.set`,
  `selections.read`, `selections.set`, `revealRange`, `setDecorations` — are
  silently accepted by the VS Code host even after the editor is closed.
- The Agda global listeners in `Main.res` (`onDidChangeTextEditorSelection`,
  `onDidChangeTextDocument`) use `setDecorations`, `revealRange`, and
  `Tokens.applyEdit` (document-level, not editor-level) on the captured
  `editor` — not `TextEditor.edit`. Those operations silently succeed on stale
  closed handles, producing no warning and no throw.
- Phase 3 attempted a listener-path trigger by calling `TextEditor.edit` (no-op
  builder) on Goals.agda while the stale captured-editor was in scope; the edit
  call returned `ok:true` and no disposed-editor text appeared in the
  extension-host log. Whether `onDidChangeTextDocument` actually fired or
  whether stale-use counters incremented was not verified, so the absence of a
  warning is not conclusive evidence about the listener path.
- The `same-document-two-groups / closeEditorsInGroup` case shows that `edit`
  returns `ok:true` (not a throw) when the underlying document is still open
  in another group — the guard is on the editor view, not the document.
- H14 reproduced the "TextEditor#edit not possible on closed editors" throw
  mechanistically, but that code path is not reached by the Agda listener
  route. The #300 disposed-editor warning origin remains unidentified by
  automated reproduction.

Cleanup: `test/tests/Test__Issue300.res` deleted; `test/TestSuiteAdapter.res`
restored from `/tmp/issue300-h14-TestSuiteAdapter.res.bak`; ReScript and webpack
rebuilt; `lib/bs/test/tests/Test__Issue300-AgdaModeVscode.cmt` removed; working
tree shows only `docs/issue#300.md` modified.

### H15: instrumented listener-path trigger after stale closed editor

Hypothesis: H14 did not decide the #300 listener route because its Phase 3 did
not verify that `onDidChangeTextDocument` actually fired. If the stale Agda
captured editor can be made closed enough that `TextEditor.edit` throws, then a
real automated text or selection event should let us directly test the listener
route: either the instrumented `Main` listeners fire and touch the stale closed
captured editor without warning, or they throw/log the disposed-editor warning.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation in `src/Main/Main.res`. Do not use any manual GUI steps.
3. Add a temporary extension-side recorder in `Main.res`, exposed through a
   temporary command such as `agda-mode.issue300-h15-dump`. Record at minimum:
   - `capturedEditorIsStaleCount`;
   - `capturedEditorEditThrowsBeforeTrigger` and exact exception text;
   - `selectionListenerEntryCount`;
   - `selectionListenerWhileCapturedStaleCount`;
   - `selectionCapturedDocumentResult` (`ok:<fileName>` or `throw:<message>`);
   - `textChangeListenerEntryCount`;
   - `textChangeListenerWhileCapturedStaleCount`;
   - `textChangeEventMatchesStateDocumentCount`;
   - exact result of each captured-editor operation inside the text-change
     listener: `IM.Input.fromTextDocumentChangeEvent`, `Tokens.applyEdit`, and
     `Goals.scanAllGoals`;
   - any caught exception text and whether extension-host output contains
     `TextEditor is closed`, `TextEditor is disposed`, `closed`, or `disposed`.
4. Wrap only the temporary instrumentation probes in catch/try blocks. Do not
   hide the real listener behavior: record whether the real call would have
   thrown, but keep the test deterministic by catching and logging exact text.
5. Recreate the H14 Phase-2 stale closed captured-editor setup:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load` so `Main.initialize` captures editor A;
   - open `test/tests/assets/Goals.agda` in the same editor group;
   - switch back to `InputMethod.agda` if needed to make `state.editor` point at
     the active editor while the listener closure still holds editor A;
   - close/replace the captured editor using the smallest H14 lifecycle that
     made `TextEditor.edit` throw on editor A;
   - before triggering the listener, prove from extension-side evidence that
     `capturedEditor !== state.editor`, the state/registry is still live, and
     a direct no-op `TextEditor.edit` probe on captured editor A throws
     `"TextEditor#edit not possible on closed editors"`.
6. Trigger a real listener event with two automated variants:
   - text-change variant: use the same H8/H12 `type` probe on the active Agda
     editor, then undo to keep fixtures clean;
   - selection variant: move/set the active editor selection using a VS Code
     command or `TextEditor.selection` assignment that H8/H12 proved emits
     `onDidChangeTextEditorSelection`.
7. After each variant, dump the recorder and capture extension-host output.
   Record whether the listener entry count increased, whether the captured
   editor was stale/closed at entry, whether the event matched
   `state.document`, and whether any exact disposed-editor warning or throw was
   observed.
8. Interpret the result:
   - `confirmed`: a real `Main` listener fires while the captured Agda editor is
     stale and `TextEditor.edit`-closed, and the listener path throws/logs the
     exact #300 disposed-editor warning.
   - `falsified`: a real `Main` listener fires while the captured Agda editor is
     stale and `TextEditor.edit`-closed, performs the captured-editor operations
     under test, and no disposed-editor warning/throw appears.
   - `inconclusive`: the setup cannot keep the extension state live while the
     captured editor is `TextEditor.edit`-closed, the automated text/selection
     trigger does not fire the listener, or extension-host warning capture is
     unreliable.
9. Write the result, raw counter values, exact warning/exception text, and
   interpretation below in this file. Do not claim no-warning behavior unless
   the listener-entry counters prove the listener actually ran.
10. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, scratch files, compiled artifacts, and any `/tmp` files.
    Leave the working tree with only the intended documentation update unless a
    permanent automated reproducer was explicitly requested.

Result:

**INCONCLUSIVE on original hypothesis; stale-but-open listener behavior observed** — The required precondition (`capturedEditorEditResult = "throw:…"`, i.e. the captured editor is edit-closed while listeners are still live) was not met: `capturedEditorEditResult` remained `'edit-returned-promise'` throughout. The "stale + edit-closed with live listeners" condition was not achieved; based on H10/H13/H14 close experiments, closing the tab that disposes the editor appears to also destroy the extension state and its registered listeners, but H15 did not attempt or instrument that close step directly. Within the stale-but-open case that was reached, all listener operations silently succeeded and no exception was thrown.

Raw dump values (from automated test, 1 passing, 2004 ms):

Baseline (after `agda-mode.load` on InputMethod.agda, before triggering listeners):
```
capturedEditorEditResult:                 'edit-returned-promise'
capturedEditorIsStaleCount:               0
selectionListenerEntryCount:              0
selectionListenerWhileCapturedStaleCount: 0
textChangeListenerEntryCount:             0
textChangeListenerWhileCapturedStaleCount: 0
textChangeEventMatchesStateDocumentCount: 0
selectionCapturedDocumentResult:          (absent)
fromTextDocumentChangeEventResult:        (absent)
applyEditResult:                          (absent)
scanAllGoalsResult:                       (absent)
caughtExceptionText:                      (absent)
```

After text-change variant (insert "x" at position 0 + undo) and selection variant (cursorRight — no additional events fired):
```
capturedEditorEditResult:                 'edit-returned-promise'
capturedEditorIsStaleCount:               6
selectionListenerEntryCount:              2
selectionListenerWhileCapturedStaleCount: 2
selectionCapturedDocumentResult:          'ok:/…/test/tests/assets/InputMethod.agda'
textChangeListenerEntryCount:             4
textChangeListenerWhileCapturedStaleCount: 4
textChangeEventMatchesStateDocumentCount: 4
fromTextDocumentChangeEventResult:        'ok'
applyEditResult:                          'ok'
scanAllGoalsResult:                       'ok'
caughtExceptionText:                      (absent)
```

Interpretation:

- `capturedEditorIsStaleCount = 6` confirms the stale condition (`capturedEditor !== state.editor`) was live across all 6 listener invocations. VS Code creates a fresh TextEditor handle when switching back to a tab — the captured handle (editorA_1) and the new state handle (editorA_2) are distinct objects, even though both point to the same open document.
- `capturedEditorEditResult = 'edit-returned-promise'` — the captured editor is stale but NOT `TextEditor.edit`-closed. Switching tabs does not close the old editor; only explicitly closing the tab does. Since the tab remained open throughout the test, `edit` returned a promise normally. The "stale + closed" condition (tab closed AND listeners still live) was not achieved in this run; based on prior close experiments (H10/H13/H14), it appears not to co-exist, but H15 did not instrument the close step to confirm this directly.
- Text-change listener: fired 4 times while stale. All three internal operations — `IM.Input.fromTextDocumentChangeEvent`, `Tokens.applyEdit`, `Goals.scanAllGoals` — returned `'ok'`. None call `TextEditor.edit`, so none can throw the disposed-editor error.
- Selection listener: fired 2 times while stale (triggered by the text-edit cursor reposition; `cursorRight` produced no event). Document access on the stale captured editor succeeded.
- `caughtExceptionText` absent across all snapshots — no exception caught anywhere in the listener path.
- No `TextEditor is closed/disposed` warning appeared in extension-host output.

H15 result: the stale-but-open listener path produced no warning or exception in this harness — all operations returned `'ok'`. The stale-and-edit-closed variant was not reached; H15 did not attempt the close step, so whether that variant would throw remains an open question. The listener path as a source of the #300 error is neither confirmed nor ruled out by this experiment alone.

Cleanup: H15 instrumentation removed from `src/Main/Main.res`; `test/tests/Test__Issue300.res` deleted; `test/TestSuiteAdapter.res` glob restored; compiled artifacts (`lib/bs/test/tests/Test__Issue300-AgdaModeVscode.cmt` and related) removed; full rebuild verified. Working tree diff is `docs/issue#300.md` only.

### H16: close/teardown ordering race after stale listener setup

Hypothesis: the missing #300 ingredient is a narrow close/teardown ordering
window: after the captured Agda editor becomes `TextEditor.edit`-closed, the
per-state global listeners may still be registered long enough to receive a
selection/text event and touch the closed captured editor. H15 did not test this
directly because it never attempted the close step; it only observed the
stale-but-open listener path.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation in `src/Main/Main.res` and `src/State/State.res` if needed.
   Do not use any manual GUI steps.
3. Add a temporary extension-side recorder exposed through a command such as
   `agda-mode.issue300-h16-dump`. Record monotonic sequence numbers for every
   event below so ordering is unambiguous:
   - H8/H12 stale setup reached: `capturedEditor !== state.editor`;
   - captured editor `TextEditor.edit` probe before close;
   - close command issued, with exact command name;
   - `onDidCloseTextDocument` / `Inputs.onCloseDocument` entry, if any;
   - state disposal / subscription-disposal entry and completion;
   - each listener entry after close attempt:
     `onDidChangeTextEditorSelection` and `onDidChangeTextDocument`;
   - captured editor operation results after close:
     `document.fileName`, `TextEditor.edit`, `IM.Input.fromTextDocumentChangeEvent`,
     `Tokens.applyEdit`, `Goals.scanAllGoals`;
   - any caught exception text and any extension-host output containing
     `TextEditor is closed`, `TextEditor is disposed`, `closed`, or `disposed`.
4. Recreate the proven stale-but-open setup:
   - open `test/tests/assets/InputMethod.agda`;
   - execute `agda-mode.load`;
   - open `test/tests/assets/Goals.agda` in the same editor group;
   - switch back to `InputMethod.agda`;
   - dump and verify `capturedEditor !== state.editor`, listeners are live, and
     a pre-close `TextEditor.edit` probe on the captured editor returns normally.
5. Run a bounded matrix of close/trigger schedules. For each row, record exact
   close command, trigger command/API, sequence log, counters, exceptions, and
   extension-host warning text:
   - close active editor, then trigger text event immediately without waiting;
   - close active editor, then trigger selection event immediately without
     waiting;
   - close active editor, then trigger text and selection events after
     microtask/0ms timeout;
   - close all editors, then trigger text/selection events immediately;
   - close editors in group, then trigger text/selection events immediately;
   - close active editor, reopen `InputMethod.agda`, then trigger text/selection
     events immediately;
   - any VS Code API/command that closes the current tab and leaves another
     Agda editor active, if available.
6. For each schedule, explicitly test the required precondition after the close
   attempt and before interpreting listener results:
   - if captured editor `TextEditor.edit` still returns normally, record
     `not-closed`;
   - if captured editor `TextEditor.edit` throws
     `"TextEditor#edit not possible on closed editors"`, record `edit-closed`;
   - if registry/state/listeners were already disposed before any post-close
     listener entry, record that ordering.
7. Interpret the result:
   - `confirmed`: a post-close listener entry occurs while the captured editor
     is `edit-closed`, and the listener path throws/logs the exact #300
     disposed-editor warning.
   - `falsified`: at least one post-close listener entry occurs while the
     captured editor is `edit-closed`, all captured-editor listener operations
     are exercised, and no disposed-editor warning/throw appears.
   - `blocked by teardown`: every schedule that makes the captured editor
     `edit-closed` disposes state/listeners before any post-close listener can
     run, while schedules that keep listeners live leave the captured editor
     not-closed.
   - `inconclusive`: the test cannot reliably issue the close/trigger sequence,
     cannot observe extension-side ordering, or cannot capture extension-host
     warning output.
8. Write the result, raw sequence log, exact close/trigger schedule table,
   exception/warning text, and interpretation below in this file.
9. Clean up the lab: remove temporary commands/log events/instrumentation,
   temporary tests, scratch files, compiled artifacts, and any `/tmp` files.
   Leave the working tree with only the intended documentation update unless a
   permanent automated reproducer was explicitly requested.

Result:

- **confirmed close/teardown gap (probe-based)** — post-close listeners fire while captured editor is edit-closed, measured via an added `TextEditor.edit` probe inside temporary instrumentation. The real listener operations (`setDecorations`, `Tokens.applyEdit`, `Goals.scanAllGoals`) were not instrumented in H16 and do not call `edit`, so this is not a full reproduction of the #300 disposed-editor warning.

**Raw sequence log (selected schedules):**

Schedule A — close (no await), trigger text immediately:
```
closeIssuedAtSeq: 1, preCloseEditResult: 'edit-returned-promise'
textPostCloseCount: 2, textFirstPostCloseSeq: 2, textFirstCapturedEditResult: 'edit-returned-promise'
textFirstDocFileName: '.../Goals.agda'
selPostCloseCount: 1, selFirstPostCloseSeq: 3, selFirstCapturedEditResult: 'edit-returned-promise'
```

Schedule B — close (no await), trigger sel immediately:
```
closeIssuedAtSeq: 1, preCloseEditResult: 'edit-returned-promise'
textPostCloseCount: 0
selPostCloseCount: 1, selFirstPostCloseSeq: 2, selFirstCapturedEditResult: 'edit-returned-promise'
```

Schedule C — await close, trigger text 0ms:
```
closeIssuedAtSeq: 1, preCloseEditResult: 'edit-returned-promise'
textPostCloseCount: 1, textFirstPostCloseSeq: 3, textFirstCapturedEditResult: 'edit-returned-promise'
textFirstDocFileName: '.../Goals.agda'
selPostCloseCount: 2, selFirstPostCloseSeq: 2, selFirstCapturedEditResult: 'edit-returned-promise'
```

Schedule D — await close, trigger text 50ms:
```
closeIssuedAtSeq: 1, preCloseEditResult: 'edit-returned-promise'
textPostCloseCount: 1, textFirstPostCloseSeq: 3, textFirstCapturedEditResult: 'edit-returned-promise'
textFirstDocFileName: '.../Goals.agda'
selPostCloseCount: 2, selFirstPostCloseSeq: 2, selFirstCapturedEditResult: 'edit-returned-promise'
```

Schedule E — await close, trigger sel 0ms:
```
closeIssuedAtSeq: 1, preCloseEditResult: 'edit-returned-promise'
textPostCloseCount: 0
selPostCloseCount: 2, selFirstPostCloseSeq: 2, selFirstCapturedEditResult: 'edit-returned-promise'
```

Schedule F — close all, trigger text:
```
closeIssuedAtSeq: 1, preCloseEditResult: 'edit-returned-promise'
textPostCloseCount: 2, textFirstPostCloseSeq: 3, textFirstCapturedEditResult: 'edit-returned-promise'
textFirstDocFileName: '.../Goals.agda'
selPostCloseCount: 2, selFirstPostCloseSeq: 2, selFirstCapturedEditResult: 'edit-returned-promise'
```

Schedule G — close, reopen InputMethod, trigger text:
```
closeIssuedAtSeq: 1, preCloseEditResult: 'edit-returned-promise'
textPostCloseCount: 0, selPostCloseCount: 0
```

**Exception/warning text (schedules C, D, E, F, G):**

`VSCode.TextEditor.edit` returns a promise synchronously (the recorder's `try/catch`
captures the synchronous return, recording `'edit-returned-promise'`). After close
is awaited, VS Code rejects those promises:

```
rejected promise not handled within 1 second:
Error: TextEditor#edit not possible on closed editors
  at Object.edit (...extensionHostProcess.js:538:9203)
  at .../dist/app.bundle.js:14274:26   ← selection listener callback
  at .../dist/app.bundle.js:14303:26   ← text listener callback
  at .../dist/app.bundle.js:14596:49   ← mark-close command
```

The `14274` and `14303` offsets are inside the H16 listener-callback probes, confirming
the listener callbacks are executing with a closed editor reference.

**Close/trigger schedule table:**

| Schedule | trigger type | await close? | delay | textPost | selPost | editor-closed in listener? |
|----------|-------------|-------------|-------|----------|---------|---------------------------|
| A | text | no | 0ms | 2 | 1 | not confirmed (promise timing) |
| B | sel | no | 0ms | 0 | 1 | not confirmed (promise timing) |
| C | text | yes | 0ms | 1 | 2 | **yes** (rejected promise from listener) |
| D | text | yes | 50ms | 1 | 2 | **yes** (rejected promise from listener) |
| E | sel | yes | 0ms | 0 | 2 | **yes** (rejected promise from listener) |
| F | text (close-all) | yes | 0ms | 2 | 2 | **yes** (rejected promise from listener) |
| G | text (reopen first) | yes | 0ms | 0 | 0 | n/a (no post-close events) |

**Interpretation:**

H16 confirms a **probe-based close/teardown gap**. The disposal gap is structural: `onCloseDocument` calls
`finalize(false)->ignore`, which starts async teardown and immediately returns.
`workbench.action.closeActiveEditor` resolves after the tab is visually closed, but
listener disposal inside `finalize` has not yet run. In that window — which persists
at least 50ms past the awaited close (schedule D) — the global `onDidChangeTextDocument`
and `onDidChangeTextEditorSelection` listeners can still fire for any document, reaching
the captured-editor reference while it is edit-closed.

Schedules C and D are the clearest evidence: even after awaiting the close command,
text and selection listeners fire with counts ≥ 1, and the `TextEditor#edit` probe
inside the listener callback throws a rejected promise (not a synchronous throw, because
`TextEditor.edit` is async — the recorder's `try/catch` is insufficient, but VS Code's
unhandled-rejection log exposes it).

Schedule G shows that after the editor is closed AND then reopened (running `initialize`
again), the race window closes: no post-close events are recorded. This suggests `finalize`
completes during the reopen's open-and-load latency, after which the old stale listeners
are properly disposed.

The `selPostCloseCount: 2` seen in schedules C–F (when only one selection trigger was
sent) reflects the close action itself generating a selection-change event (Goals.agda
becomes active), which the still-live listener also records.

The measured ordering: `workbench.action.closeActiveEditor` resolves → listeners still
live → external event fires → listener callback executes with closed editor → at least
50ms later (schedule D) the gap is still open. The `->ignore` on `finalize` in
`Inputs.onCloseDocument` is where the gap originates.

**Cleanup:** All H16 instrumentation (module-level refs, three temporary commands,
listener-callback probes, `onCloseDocument` seq recorder, `h16_capturedEditorRef`
assignment) was removed from `src/Main/Main.res`. `test/tests/Test__Issue300.res` and
`lib/bs/test/tests/Test__Issue300-AgdaModeVscode.cmt` were deleted.
`test/TestSuiteAdapter.res` glob restored to `"**/*.js"`.

### H17: real listener operations inside the H16 teardown window

Hypothesis: H16 found the missing close/teardown race window, but only the
temporary `TextEditor.edit` probe proved that the captured editor is closed in
that window. The next question is whether the real #300 listener operations
(`TextEditor.document`, `IM.Input.fromTextDocumentChangeEvent`,
`Tokens.applyEdit`, `Goals.scanAllGoals`, `setDecorations`, `revealRange`) emit
the exact disposed-editor warning when they run inside the same post-close
window.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation in `src/Main/Main.res` and any directly needed callee modules
   (`Tokens.res`, `Goals.res`, `Editor.res`) to observe real operations. Do not
   use manual GUI steps.
3. Reuse H16's clearest schedules as the primary path:
   - schedule C: await `workbench.action.closeActiveEditor`, trigger text at
     0ms;
   - schedule D: await `workbench.action.closeActiveEditor`, trigger text after
     50ms;
   - schedule E: await `workbench.action.closeActiveEditor`, trigger selection
     at 0ms;
   - schedule F: await `workbench.action.closeAllEditors`, trigger text at 0ms.
   Keep schedules A/B/G only as controls if useful.
4. Add a recorder exposed through a temporary command such as
   `agda-mode.issue300-h17-dump`. Record sequence numbers and results for:
   - close command issued/resolved;
   - listener entry after close for selection/text;
   - `capturedEditor !== state.editor`;
   - captured editor closed state, measured by a single quarantined
     `TextEditor.edit` probe before the real operation block. This probe is only
     a state marker; do not count its rejection as #300 reproduction;
   - `VSCode.TextEditor.document(editor)` in the selection listener;
   - `IM.Input.fromTextDocumentChangeEvent(editor, event)`;
   - `Tokens.applyEdit(editor, event)`;
   - `Goals.scanAllGoals(editor, changes)`;
   - any `setDecorations` call reached by token/highlight handling;
   - any `revealRange` call reached by goal/cursor handling;
   - exact caught exception text, rejected promise text, and extension-host
     output containing `TextEditor is closed`, `TextEditor is disposed`,
     `closed`, or `disposed`.
5. The real listener operations must be invoked as they normally are in
   `Main.res`; do not replace them with synthetic `edit` calls. If wrapping is
   required to keep the test deterministic, wrap immediately around the real
   operation and record whether it returned, threw synchronously, or returned a
   promise that later rejected.
6. Capture extension-host output for the full run, including unhandled promise
   rejections. Search it separately for:
   - the quarantined `TextEditor.edit` probe rejection;
   - warnings/exceptions whose stack points to a real listener operation, not
     the probe.
7. Interpret the result:
   - `confirmed`: inside an H16 post-close listener entry, at least one real
     listener operation emits/logs/throws the exact #300 disposed-editor warning
     while the captured editor is closed.
   - `falsified`: inside an H16 post-close listener entry, the captured editor is
     closed according to the quarantined marker probe, all real listener
     operations are exercised, and none emits/logs/throws the disposed-editor
     warning.
   - `probe-only`: H16's close/teardown window and edit-probe rejection reproduce
     again, but the test cannot prove that the real listener operations ran or
     cannot distinguish their output from the probe output.
   - `inconclusive`: the H16 window cannot be reproduced reliably, warning
     capture is unreliable, or the instrumentation changes the listener path too
     much to trust the result.
8. Write the result, schedule table, raw operation results, exact
   exception/warning text, and interpretation below in this file. Be explicit
   about whether the warning came from the quarantined `edit` probe or from a
   real listener operation.
9. Clean up the lab: remove temporary commands/log events/instrumentation,
   temporary tests, scratch files, compiled artifacts, and any `/tmp` files.
   Leave the working tree with only the intended documentation update unless a
   permanent automated reproducer was explicitly requested.

Result:

- **falsified (for the exercised operations)** — inside H16 post-close listener entries, the four operations that were reached — `TextEditor.document`, `IM.Input.fromTextDocumentChangeEvent`, `Tokens.applyEdit`, `Goals.scanAllGoals` — all returned `ok` with no synchronous throw and no `threw` entry in the log. The only `TextEditor#edit` rejections in extension-host output originated from the quarantined edit probe. `setDecorations` and `revealRange` were not reached in this test setup (`setDecCount` and `revealRangeCount` remained 0 across all schedules: the document-guard in `applyEdit` skips token updates when the event document differs from the captured editor's document, and `revealRange` is only called when `cursorWasWithinRewrites` is true, which was not the case here).

**Raw operation logs:**

Schedule C — await close, trigger text at 0ms:
```
text-entry seq:1 stale:true editProbe:returned-promise
fromTextDocumentChangeEvent: ok len:0
applyEdit: ok
scanAllGoals: started
scanAllGoals: completed
sel-entry seq:2 stale:true editProbe:returned-promise
sel-document: .../InputMethod.agda
text-entry seq:3 stale:true editProbe:returned-promise
fromTextDocumentChangeEvent: ok len:0
applyEdit: ok
scanAllGoals: skipped (empty changes)
setDecCount: 0
revealRangeCount: 0
```

Schedule D — await close, trigger text at 50ms:
```
sel-entry seq:1 stale:true editProbe:returned-promise
sel-document: .../InputMethod.agda
text-entry seq:2 stale:true editProbe:returned-promise
fromTextDocumentChangeEvent: ok len:0
applyEdit: ok
scanAllGoals: started
scanAllGoals: completed
text-entry seq:3 stale:true editProbe:returned-promise
fromTextDocumentChangeEvent: ok len:0
applyEdit: ok
scanAllGoals: skipped (empty changes)
setDecCount: 0
revealRangeCount: 0
```

Schedule E — await close, trigger selection at 0ms:
```
sel-entry seq:1 stale:true editProbe:returned-promise
sel-document: .../InputMethod.agda
sel-entry seq:2 stale:true editProbe:returned-promise
sel-document: .../InputMethod.agda
setDecCount: 0
revealRangeCount: 0
```

Schedule F — close all, trigger text at 0ms:
```
(empty log — no listener entries)
setDecCount: 0
revealRangeCount: 0
```

**Exception/warning text:**

All `rejected promise not handled within 1 second: Error: TextEditor#edit not possible on closed editors` entries in extension-host output trace back to `dist/app.bundle.js:14268:22` (text listener) or `14243:22` (selection listener) — both inside the quarantined edit probe instrumentation. No stack trace points to `fromTextDocumentChangeEvent`, `applyEdit`, `scanAllGoals`, `setDecorations`, or `revealRange`.

**Schedule table:**

| Schedule | trigger type | await close? | delay | listener entries | stale? | probe | real ops threw? | setDecCount | revealRangeCount |
|----------|-------------|-------------|-------|-----------------|--------|-------|-----------------|-------------|-----------------|
| C | text | yes | 0ms | 3 (2 text, 1 sel) | yes | returned-promise | **no** | 0 | 0 |
| D | text | yes | 50ms | 3 (2 text, 1 sel) | yes | returned-promise | **no** | 0 | 0 |
| E | sel | yes | 0ms | 2 (2 sel) | yes | returned-promise | **no** | 0 | 0 |
| F | text | close-all | 0ms | 0 | n/a | n/a | n/a | 0 | 0 |

**Interpretation:**

H17 is **falsified for the exercised operations**: inside the H16 post-close teardown window, the real listener operations that were reached — `TextEditor.document`, `IM.Input.fromTextDocumentChangeEvent`, `Tokens.applyEdit`, `Goals.scanAllGoals` — all completed normally without throwing. The `stale:true` flag in every C/D/E entry confirms the stale-editor branch was entered. The captured editor being edit-closed is shown by the extension-host unhandled-rejection output: every `rejected promise not handled within 1 second: Error: TextEditor#edit not possible on closed editors` stack traces to the quarantined probe lines in `dist/app.bundle.js` (offsets `14268:22` and `14243:22`), not to any real listener operation. None of the real operations produced a synchronous throw or a rejected promise traceable to them. `setDecorations` and `revealRange` were not reached (see `setDecCount:0`, `revealRangeCount:0` above).

Schedule F's empty log shows that `workbench.action.closeAllEditors` completes teardown fully (listeners unsubscribed) before Goals.agda can be reopened and trigger events — consistent with the H16 finding that the race window is narrower under `closeAllEditors` than under `closeActiveEditor`.

`setDecCount:0` across all schedules: `Tokens.applyEdit` ran but `generateHighlighting` was not called, because the text-change event came from Goals.agda while the captured state belongs to InputMethod.agda — the document identity check inside `applyEdit` causes it to skip token updates silently without throwing. `revealRangeCount:0`: `setCursor` is only called when `cursorWasWithinRewrites` is true, which requires a goal rewrite to have moved past the cursor — not the case in this test setup.

The combined H14+H17 picture: `TextEditor.edit` is the only VS Code API that throws on a closed editor handle (H14). Of the real listener operations exercised in H17 (`TextEditor.document`, `fromTextDocumentChangeEvent`, `Tokens.applyEdit`, `Goals.scanAllGoals`), all completed normally inside the teardown window. `setDecorations` and `revealRange` were not reached in this test setup and remain untested in the stale-and-edit-closed condition. The H16 teardown race window is real, but within it the exercised operations do not trigger the #300 disposed-editor warning.

**Cleanup:** All H17 instrumentation (`h17SetDecCount` in `src/Editor.res`, `h17RevealRangeCount` in `src/Goals.res`, H17 recorder and commands in `src/Main/Main.res`, `test/tests/Test__Issue300.res`, `lib/bs/test/tests/Test__Issue300-AgdaModeVscode.cmt`) was removed. `test/TestSuiteAdapter.res` glob restored to `"**/*.js"`.

### H18: force setDecorations and revealRange inside the H16 teardown window

Hypothesis: H17 falsified the reached listener operations, but two UI-facing
operations from the candidate #300 surface were not exercised:
`TextEditor.setDecorations` and `TextEditor.revealRange`. The disposed-editor
warning may still come from one of those operations when it runs against the
closed captured editor during the H16 post-close listener window.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation in `src/Main/Main.res`, `src/Editor.res`, `src/Tokens.res`,
   and `src/Goals.res` as needed. Do not use manual GUI steps.
3. Reuse H16's clearest schedules:
   - schedule C: await `workbench.action.closeActiveEditor`, trigger text at
     0ms;
   - schedule D: await `workbench.action.closeActiveEditor`, trigger text after
     50ms;
   - schedule E: await `workbench.action.closeActiveEditor`, trigger selection
     at 0ms.
4. Keep one quarantined `TextEditor.edit` probe as a closed-editor marker, but
   do not count its rejection as #300 reproduction. Record its rejection stack
   separately from real operation stacks.
5. Force `setDecorations` through the least invasive real path first:
   - prefer a same-document text-change event for the captured state's document,
     so `Tokens.applyEdit` does not skip on document identity;
   - if the ordinary event still cannot reach highlighting, temporarily expose a
     narrow extension-side command that calls the existing token decoration path
     (`Tokens.applyDecorations` / `Editor.Decoration.apply`) with the captured
     state/editor inside the H16 post-close window;
   - record whether this is the ordinary listener path or a forced decoration
     command path.
6. Force `revealRange` through the least invasive real path first:
   - prefer an existing goal/cursor operation that calls
     `Goals.setCursorByIndex` or another existing `revealRange` path on the
     captured state/editor;
   - if normal goal movement cannot reach `revealRange`, temporarily expose a
     narrow extension-side command that calls the existing reveal path with the
     captured state/editor inside the H16 post-close window;
   - record whether this is the ordinary listener path or a forced reveal command
     path.
7. For each operation and schedule, record:
   - listener/command entry sequence after close;
   - `capturedEditor !== state.editor`;
   - quarantined edit probe result and rejection stack;
   - `setDecorations` entered? returned? threw? rejected?;
   - `revealRange` entered? returned? threw? rejected?;
   - exact extension-host output containing `TextEditor is closed`,
     `TextEditor is disposed`, `closed`, or `disposed`;
   - whether any warning stack points to `setDecorations`/decoration path or
     `revealRange`/goal path rather than the quarantined edit probe.
8. Interpret the result:
   - `confirmed`: inside the H16 post-close window, real `setDecorations` or
     real `revealRange` emits/logs/throws the exact #300 disposed-editor warning
     while the captured editor is closed.
   - `falsified`: both `setDecorations` and `revealRange` are exercised inside
     the H16 post-close window while the captured editor is closed, and neither
     emits/logs/throws the disposed-editor warning.
   - `partially falsified`: one of the two operations is exercised and does not
     emit/log/throw, but the other cannot be reached.
   - `inconclusive`: the H16 window cannot be reproduced reliably, the operation
     can only be exercised through a synthetic path that is too far from real
     behavior, or warning output cannot be separated from the edit probe.
9. Write the result, schedule table, raw operation results, exact warning text,
   and interpretation below in this file. Be explicit about whether each
   operation was reached through the normal listener path or a forced command
   path.
10. Clean up the lab: remove temporary commands/log events/instrumentation,
    temporary tests, scratch files, compiled artifacts, and any `/tmp` files.
    Leave the working tree with only the intended documentation update unless a
    permanent automated reproducer was explicitly requested.

Result:

- **falsified** — inside the H16 post-close window, both `setDecorations` and
  `revealRange` were exercised via forced command paths while the captured
  Goals.agda editor was closed, and neither threw nor emitted the disposed-editor
  warning.

**Stale setup.** `makeAndLoad("Goals.agda")` (real Agda decorations and goals
loaded) → open `InputMethod.agda` → switch back to `Goals.agda` → captured
editor is the fully-loaded Goals.agda TextEditor. Forced commands
`agda-mode.issue300-h18-force-decorate` and `agda-mode.issue300-h18-force-reveal`
were registered in `initialize` via `subscribe`, closing over `(state, editor)`.

**Schedule table.**

| Schedule | Timing | editProbe | `applyDecorations` | `setCursorByIndex` | setDecCount | revealRangeCount |
|----------|---------|-----------|--------------------|---------------------|-------------|------------------|
| C | 0 ms after close | `returned-promise` | ok | ok | 5 | 1 |
| D | 50 ms after close | `returned-promise` | ok | ok | 5 | 1 |
| E | 0 ms + open InputMethod.agda | `returned-promise` | ok | ok | 5 | 1 |

**Raw operation results.**
- `force-decorate editProbe: returned-promise` and `applyDecorations: ok` in all
  three schedules; `Editor.Decoration.h18SetDecCount` = 5 (Goals.agda carried
  five distinct decoration types from Agda highlighting; `applyDecorations`
  iterated all five and called `TextEditor.setDecorations` once each).
- `force-reveal editProbe: returned-promise` and `setCursorByIndex: ok` in all
  three schedules; `Goals.h18RevealRangeCount` = 1 (goal index 0 found,
  `revealRange` called once).
- Both forced commands were still callable at 50 ms (schedule D), consistent with
  the H16 ≥ 50 ms teardown window.

**Extension-host warning output.** Two rejections per schedule — one from the
force-decorate probe and one from the force-reveal probe — both pointing to the
quarantined lines in `dist/app.bundle.js`:

```
rejected promise not handled within 1 second: Error: TextEditor#edit not possible on closed editors
stack trace: Error: TextEditor#edit not possible on closed editors
    at Object.edit (.../extensionHostProcess.js:542:9203)
    at .../dist/app.bundle.js:2:246855
    ...
```

No warning from the `setDecorations` path or the `revealRange`/goal path.

**Interpretation.** The captured Goals.agda editor was edit-closed (confirmed by
the edit-probe unhandled rejections tracing to the quarantined probe offsets in
`dist/app.bundle.js`). Inside that window, `Tokens.applyDecorations` iterated
five decoration types and called `TextEditor.setDecorations` five times without
throwing or logging any warning; `Goals.setCursorByIndex` located goal index 0
and called `TextEditor.revealRange` once without throwing or logging any warning.
The hypothesis is **falsified via forced command paths**: neither `setDecorations`
nor `revealRange` is the source of the #300 disposed-editor warning when called
against a closed captured editor.

**Cleanup note.** Temporary instrumentation (`h18SetDecCount` in
`Editor.Decoration`, `h18RevealRangeCount` in `Goals`, `h18LogRef` / forced
commands in `Main`), `test/tests/Test__Issue300.res`, and the compiled artifact
`lib/bs/test/tests/Test__Issue300-AgdaModeVscode.cmt` were all removed after
recording these results. `git status` shows only `docs/issue#300.md` modified.

### H19: reproduce the reported Unicode IM warning stream directly

Hypothesis: H18 exhausted the stale captured-editor operation surface we had
been probing, but #300 may require the full Unicode input method workflow rather
than a single isolated `TextEditor` operation. The missing ingredient may be
accumulated IM/decoration activity during repeated real `\` activation and
symbol-sequence keypresses, with the warning only visible in the extension-host
log channel used by the reported WSL/VS Code Server setup.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation in the smallest set of files needed. Do not use manual GUI
   steps.
3. Configure the run to match the issue report as closely as the automated test
   harness allows:
   - ensure `agdaMode.inputMethod.enable` is `true`;
   - open a real `.agda` file through VS Code;
   - load it through the extension before the IM stress phase;
   - activate Unicode IM with the same workflow the user uses, starting from a
     real editor text insertion of `\` where possible;
   - type symbol sequences that should rewrite, for example `\l` to `λ`, through
     real text edits where possible.
4. Capture the real extension-host warning stream, not only test assertions:
   - capture test runner stdout/stderr;
   - locate and read the VS Code extension-host log directory for the test run
     if available;
   - search every captured output file for exact or near-exact warning text:
     `TextEditor is closed/disposed`, `TextEditor is closed`,
     `TextEditor is disposed`, `closed editors`, `disposed`.
5. Add temporary extension-side counters that can explain warning volume without
   changing behavior:
   - number of selection listener entries and text-change listener entries;
   - number of stale listener entries (`capturedEditor !== state.editor`);
   - number of IM activations/deactivations;
   - number of IM instances created/redecorated/destroyed;
   - number of `Editor.Decoration.apply` calls and decoration-type disposals;
   - number of editor IM rewrites and `Editor.Text.batchReplace` calls;
   - number of states and subscriptions live after each stress phase.
6. Run an automated stress matrix that starts small and then grows:
   - baseline: one load, one IM activation, one rewrite sequence;
   - repeated keypresses: 50 rewrite sequences in the same editor;
   - repeated activation cycles: 50 activate/rewrite/deactivate cycles;
   - tab-switch stress: repeat the H8 same-group replacement before each
     activation cycle, then type the rewrite sequence;
   - close-teardown stress: repeat the H16 close/0ms and close/50ms schedules
     with IM already active, then type or force the next IM input event.
7. For every phase, record:
   - exact commands/editor operations used to drive the IM workflow;
   - warning counts per keypress/cycle from the captured log files;
   - the first and last exact warning lines if any are observed;
   - whether warning count grows with cycle number;
   - all extension-side counters from step 5;
   - whether any warning stack/output can be correlated with IM activation,
     decoration application/disposal, rewrite handling, or listener entry.
8. Interpret the result:
   - `confirmed`: the automated test emits the exact
     `TextEditor is closed/disposed` warning during the Unicode IM workflow,
     without relying on a quarantined probe, and the reproduction is stable
     across repeated runs.
   - `partially confirmed`: the exact warning appears automatically but only in
     one stress phase, only intermittently, or without enough counters to explain
     the source.
   - `falsified for local test harness`: all issue-faithful IM stress phases run
     automatically with bounded counters and no exact disposed-editor warning in
     any captured extension-host output.
   - `inconclusive`: the harness cannot drive the real Unicode IM workflow, the
     extension-host log channel cannot be captured, or temporary instrumentation
     materially changes the IM behavior.
9. Write the result, stress matrix, raw counter table, captured warning text,
   and interpretation below in this file. Be explicit about whether the reported
   warning was reproduced automatically and deterministically.
10. Clean up the lab: remove temporary counters/log events/instrumentation,
    temporary tests, scratch files, compiled artifacts, and any `/tmp` or VS Code
    log copies made for the experiment. Leave the working tree with only the
    intended documentation update unless a permanent automated reproducer was
    explicitly requested.

### H19 Results (2026-06-21)

**Classification: partially confirmed** — `TextEditor is closed/disposed` reproduced
automatically in the test harness in one run via the H8 stale-capture path. Stable
reproduction across repeated runs was not recorded; the single warning line in exthost.log
is the only captured evidence.

#### Stress matrix

| Phase | Description | selCount | textCount | staleSelCount | staleTextCount | decApplyCount |
|-------|-------------|----------|-----------|---------------|----------------|---------------|
| 1 | baseline (1 IM cycle) | 1 | 3 | 0 | 0 | 2 |
| 2 | repeated keypresses (20 cycles, same editor) | 20 | 41 | 0 | 0 | 40 |
| 3 | repeated activation cycles (20 cycles) | 20 | 41 | 0 | 0 | 40 |
| 4 | tab-switch stress (H8: capturedEditor ≠ state.editor) | 20 | 41 | 20 | 41 | 40 |
| 5 | close-teardown (H16: IM active at close) | 0 | 3 | 0 | 0 | 0 |

All 5 phases passed (5 passing, 938ms total, exit code 0). No exceptions or test failures.

#### Captured warning

Found in VS Code extension-host log
(`/tmp/agda-1782053383-941428/user-data/logs/20260621T224943/window1/exthost/exthost.log`,
line 117 of 119):

```
2026-06-21 22:49:45.952 [warning] TextEditor is closed/disposed
```

The warning appears in the extension-host log channel (not test stdout), matching the
original #300 report's observation mechanism exactly.

#### Timing analysis

- Extension host activated at `22:49:45.120` (exthost.log line 114)
- Warning at `22:49:45.952` — 832ms after activation, 106ms before process exit (`22:49:46.058`)
- Phase durations: phase 1 (117ms), phase 2 (146ms), phase 3 (122ms), phase 4 (230ms),
  phase 5 (148ms); each preceded by `beforeEach` (`closeAllEditors` + `h19Reset`)
- The warning timestamp falls within phase 4's execution window (tab-switch stress)

#### Interpretation

Phase 4 creates the H8 stale-capture condition by:
1. Opening `InputMethod.agda` → `capturedEditor = editor1` (captured in global listener closure)
2. Switching to `Goals.agda` then back → VS Code disposes `editor1`; `state.editor = editor2`
3. Running 20 IM cycles — each cycle triggers text-change events

In this configuration text-change events enter the global listener with
`capturedEditor = editor1` (disposed) and `state.editor = editor2` (live); the counters
show `textCount: 41, staleTextCount: 41, decApplyCount: 40`. The stale-decoration path
(`Tokens.applyEdit` → `Tokens.applyDecorations` → `Editor.Decoration.apply` →
`capturedEditor.setDecorations(...)`) was active for all 40 decoration calls. One
`TextEditor is closed/disposed` warning was captured in exthost.log during this phase.
The data is consistent with the warning originating from this path, but exthost.log
records only a single log line with no stack — not 40 separate entries — so it is not
established that every decoration call produced the warning; VS Code may rate-limit or
deduplicate the log output.

Phase 5 (H16 close-teardown): `textCount: 3` but `decApplyCount: 0` — the teardown-gap
text events do not reach `setDecorations` (no decoration apply path is triggered by
`keyUpdateEditorIM` alone without an active IM rewrite cycle). No warning produced.

**Automatic reproduction confirmed; likely path identified.** The warning was
automatically reproduced in the test harness during the H8 tab-switch stress phase. The
most likely path is `setDecorations` called on a tab-switch-disposed `TextEditor` handle:
the `editor` captured in the `onDidChangeTextDocument` closure at `initialize` time in
`src/Main/Main.res` is not updated when the user switches away and back to a tab (VS Code
issues a fresh `TextEditor` handle, making the captured one disposed). The captured
exthost.log line carries no stack trace, so this assignment is correlational — the
decoration-apply counter and stale-use counter both peaked in phase 4, and no other
`TextEditor` API call is known to be invoked with the disposed handle in this path. The
H16 close-teardown shape does not independently produce the warning in this test setup.

### H20: establish repeatability and isolate the warning source

Hypothesis: H19's single exact warning is a real automatic #300 reproduction,
but the evidence is still partial because it was recorded in only one run and
the warning line has no stack. The warning should be repeatable under the H8
tab-switch Unicode IM stress path, and a narrower tab-switch-disposed
`setDecorations` isolation phase should be sufficient to emit the same warning
without relying on the rest of the IM rewrite workflow. The reported "hundreds
per keypress" flooding may require longer stress, more repeated tab switches, or
multiple stale listener states.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation in the smallest set of files needed. Do not use manual GUI
   steps.
3. Reuse H19's log-capture method:
   - capture test runner stdout/stderr;
   - locate and read the VS Code extension-host log directory for each run;
   - count exact `TextEditor is closed/disposed` lines separately from
     `TextEditor is closed`, `TextEditor is disposed`, `closed editors`, and
     `disposed`;
   - record the log path, first/last warning line, and warning count for each
     run.
4. Add temporary extension-side run/phase markers with monotonically increasing
   IDs so warning timestamps can be assigned to phases without relying on wall
   clock guessing. Record markers before and after:
   - H8 tab-switch setup;
   - each IM cycle batch;
   - each `Tokens.applyDecorations` / `Editor.Decoration.apply` call on a stale
     captured editor;
   - each forced isolation command call from step 7.
5. Repeat the H19 phase-4 tab-switch IM stress in fresh VS Code test processes:
   - run at least 10 independent repetitions;
   - each repetition should perform the same H8 setup, then 20 IM cycles;
   - record `textCount`, `staleTextCount`, `decApplyCount`,
     warning count, and whether the exact warning appears.
6. Add a longer stress variant to test the reported flooding:
   - one run with 200 IM cycles after one H8 tab-switch setup;
   - one run with 50 repeated H8 tab-switch setups, each followed by 5 IM cycles;
   - one run with at least 3 Agda states initialized, then stale each one with
     tab switches before driving IM cycles in one active editor;
   - record whether warning count scales with cycles, stale states, or repeated
     tab switches.
7. Add a narrow tab-switch-disposed `setDecorations` isolation phase:
   - create the H8 stale-capture condition for `InputMethod.agda`;
   - keep a captured `editor1` and the fresh `state.editor = editor2`;
   - register a temporary extension-side command that calls only the existing
     decoration application path on the captured `editor1`, using a known
     decoration type and range;
   - do not run IM rewrite handling, `Tokens.applyEdit`, `Goals.scanAllGoals`,
     `revealRange`, or any quarantined `TextEditor.edit` probe in this phase;
   - call the command repeatedly, then search exthost.log for the exact warning.
8. Add a control isolation phase:
   - call the same decoration application path on the fresh live `state.editor`;
   - use the same decoration type/range and repeat count as step 7;
   - record whether any warning appears.
9. Interpret the result:
   - `confirmed deterministic reproduction`: the exact warning appears in every
     repeated H19-style run, without quarantined probes or manual steps.
   - `confirmed source`: the exact warning appears in the stale captured-editor
     `setDecorations` isolation phase and does not appear in the live-editor
     control phase.
   - `confirmed flooding`: warning count grows substantially with cycle count,
     stale-state count, or repeated tab switches, approaching the reported
     "hundreds per keypress" behavior.
   - `partially confirmed`: the warning appears automatically but not in every
     repetition, or the source remains only correlational.
   - `inconclusive`: warning capture is unstable, phase markers cannot be
     correlated with log output, or instrumentation changes the IM behavior.
   - `falsified for local harness`: no exact warnings appear across repeated
     H19-style runs, long stress, or isolation phases.
10. Write the result, repetition table, long-stress table, isolation/control
    table, exact warning counts, raw warning lines, and interpretation below in
    this file. Be explicit about which claims are deterministic, which are only
    partial, and whether flooding was reproduced.
11. Clean up the lab: remove temporary commands/counters/log events/
    instrumentation, temporary tests, scratch files, compiled artifacts, and any
    `/tmp` or VS Code log copies made for the experiment. Leave the working tree
    with only the intended documentation update unless a permanent automated
    reproducer was explicitly requested.

### H20 Results (2026-07-08)

**Classification: partially confirmed source, falsified for the naturalistic IM
path (local harness).** A narrow isolation test that calls `setDecorations`
directly on a tab-switch-disposed captured `TextEditor` handle — with no IM,
no `Tokens.applyEdit`, no `Goals.scanAllGoals` involved — reproduced the exact
`TextEditor is closed/disposed` warning deterministically (40/40 calls across
two independent repetitions), while the same call on the live editor produced
it zero times (0/40). This is the strongest causal evidence gathered so far
for the mechanism. However, driving the warning through the *real* listener
path (the actual Unicode IM workflow used by H19) did not reproduce it again
in this round: 0 warnings across 10 independent repeatability repetitions plus
three long-stress variants, despite hundreds of stale decoration-apply calls
in each. H19's single automatic reproduction was not repeated.

#### Method note

Test-side infrastructure: `test/TestSuiteAdapter.res` gained three env-based
filters for this lab session (`AGDA_TEST_GLOB` to run only
`test/tests/Test__Issue300.res`, `AGDA_TEST_GREP` to select individual phases,
`AGDA_TEST_TIMEOUT` to raise the per-test mocha timeout for the long-stress
phases) — all three default to running the full suite unfiltered at the
original timeout. `src/Editor.res`
and `src/Main/Main.res` gained temporary counters
(`selCount`/`staleSelCount`/`textCount`/`staleTextCount`/`decApplyCount`,
read/reset via two temporary commands) and two temporary commands,
`agda-mode.issue300-h20-isolate` / `-control`, that call `setDecorations`
directly on, respectively, the most recently captured closure editor and the
most recently created state's live editor — the same `Editor.Decoration.apply`
path the real listener code uses, with nothing else involved. All of this was
removed at the end of the lab session (see Cleanup below).

The repeatability phase reused the H19 phase-4 recipe (H8 tab-switch, then 20
real Unicode IM cycles: activate, insert a rewrite-triggering character, escape,
clear the line) but is a reconstruction, not the literal H19 test file (which
had already been deleted per H19's own cleanup step) — so this is not a
byte-for-byte replication of H19's exact code path, only of its documented
recipe.

#### Repeatability (H8 tab-switch + 20 IM cycles, 10 independent VS Code processes)

| Rep | selCount | staleSelCount | textCount | staleTextCount | decApplyCount | exact warning |
|-----|----------|---------------|-----------|-----------------|----------------|---------------|
| 1–10 | 40 | 40 | 61 | 61 | 40 | 0 (all 10 reps identical counters, 0 warnings each) |

All 10 repetitions produced identical counters and zero occurrences of
`TextEditor is closed/disposed` in exthost.log. H19's one-off reproduction did
not repeat under this reconstruction of its recipe.

#### Long-stress variants (single process each)

| Variant | selCount | staleSelCount | textCount | staleTextCount | decApplyCount | exact warning |
|---------|----------|---------------|-----------|-----------------|----------------|---------------|
| 200 IM cycles, 1 H8 setup | 400 | 400 | 601 | 601 | 400 | 0 |
| 50× H8 setup, 5 IM cycles each | 500 | 500 | 750 | 750 | 550 | 0 |
| 3 simultaneously-stale states, 20 cycles in one | 120 | 120 | 183 | 183 | 40 | 0 |

No warning appeared in any of the three long-stress variants, despite up to
550 stale decoration-apply calls in a single run. The reported "hundreds per
keypress" flooding behaviour was not observed or approached under any of these
conditions; if anything, volume did not correlate with warning occurrence at
all in this round (0 warnings across 1,390 combined stale decoration-apply
calls in the naturalistic path).

#### Isolation / control (narrow `setDecorations`-only causal test)

| Phase | Editor | Repetitions | Calls/rep | Exact warning |
|-------|--------|--------------|-----------|---------------|
| Isolation | stale captured (tab-switch-disposed) | 2 independent processes | 20 | 20/20 both times |
| Control | live (current `state.editor`) | 2 independent processes | 20 | 0/20 both times |

Raw captured lines (isolation run, `/tmp/agda-1783482513-669880/user-data/logs/20260708T114833/window1/exthost/exthost.log`,
lines 117–136 of 138): 20 consecutive
```
2026-07-08 11:48:35.158 [warning] TextEditor is closed/disposed
...
2026-07-08 11:48:35.466 [warning] TextEditor is closed/disposed
```
lines, timed to the isolation phase's execution window (before the control
phase ran); zero such lines appear afterward, during or after the control
phase's 20 calls. A second independent repetition reproduced the same 20/0
split exactly.

This is a clean causal result: calling `setDecorations` on a tab-switch-disposed
`TextEditor` handle, with nothing else involved, reproduces the exact #300
warning deterministically; the same call on the live handle never does.

**Caveat — a contaminated third run.** An earlier combined run (long-stress
variants + isolation + control all in one process) produced 40 warnings split
evenly 20/20 between the isolation and control phases, which looked at the
time like it falsified the live/stale distinction. Investigation found the
cause: `issue300H20LatestClosureEditor` / `issue300H20LatestState` (the refs
the isolate/control commands read) are only updated inside `initialize`, which
only runs on a document's *first* state creation; since Registry state persists
across `it` blocks within one process and the preceding long-stress-3states
test was the most recent to trigger a first-time `initialize` (for `Give.agda`),
both commands were silently still operating on `Give.agda`'s handles, not
`InputMethod.agda`'s, for the rest of that process's lifetime. That run's
control phase therefore hit `Give.agda`'s `state.editor` — which, because
`Give.agda` had not been the active tab since the 3-states test switched away
from it, may itself have gone stale from simple backgrounding rather than an
actual close. This is a plausible secondary finding (a "live" reference can go
stale too, if its listener's `onOpenEditor` refresh has not fired since the
last tab switch) but is not confirmed independently — the two clean,
correctly-scoped isolation/control repetitions reported above are the ones
this section's classification is based on, not the contaminated run.

#### Interpretation

Per the handoff's classification scheme, this is a mix of `confirmed source`
(isolation vs. control) and `falsified for local harness` (repeatability and
all three long-stress variants): the causal mechanism — `setDecorations` on a
disposed captured `TextEditor` handle — reliably produces the exact warning in
isolation, but the real Unicode IM listener path did not reproduce it again in
this round despite substantially more stress than H19 applied. This does not
retract H19's single reproduction (a real occurrence, not fabricated), but it
means the naturalistic path's reproduction rate looks very low/rare rather
than "warning appears automatically in the harness" in any dependable sense.
Something about the real listener path's surrounding work (token rebasing,
`Goals.scanAllGoals`, the specific decoration ranges/types produced by actual
highlighting output vs. this test's fixed single-character range) appears to
make triggering the warning far less likely than the bare isolated call — why
is not established by this round of testing.

#### Cleanup

All temporary instrumentation was removed after recording these results:
the env-based filters in `test/TestSuiteAdapter.res`, the counters/commands in
`src/Editor.res` and `src/Main/Main.res`, and `test/tests/Test__Issue300.res`
itself. The compiled artifact `lib/bs/test/tests/Test__Issue300-AgdaModeVscode.cmt`
was initially missed (a prior lab session had the same gap) and was found and
removed in a follow-up pass. `git status` shows only `docs/issue#300.md`
modified.

## Conclusion

#300 and #318 are **not the same bug** and don't share a root cause:

- #318 fixed the "deltas accumulate and are never rebased" mechanism — a
  `Tokens`-level bookkeeping bug.
- #300's reported symptom is a disposed-editor throw/warning. H19 automatically
  reproduced the exact `TextEditor is closed/disposed` warning in exthost.log
  during the H8 tab-switch stress phase. H20 then isolated `setDecorations`
  called on a tab-switch-disposed captured editor from every other operation
  and reproduced the warning deterministically (40/40 calls across two
  independent runs, vs. 0/40 on a live handle) — confirming that mechanism
  causally, though whether it explains any particular naturally-occurring
  warning (H19's included) remains correlational, since log lines carry no
  stack. H20 also drove the real IM listener path far harder than H19 (10
  independent repeatability runs, three long-stress variants) without
  reproducing the warning again that way. H21 then tried to explain why and
  concluded it was about synchronous-vs-decoupled calling — but that
  conclusion was based on tests that never loaded a real Agda file, so
  `Tokens`'s (stale-editor-using) decoration calls never actually fired; H21
  had unknowingly been testing `IM.res`'s decoration (always the live editor)
  instead. H22 corrected this: with a real Agda file loaded, staled via tab
  switch, and typed, the real listener reproduced the exact warning
  **synchronously**, inside its own call stack, 5/5 times across two
  independent runs — an automatic natural-path reproduction through the real
  listener path, though still driven by the test helper flow rather than
  manual interactive use. Flooding was not reproduced: the stale-call count held fixed at 5
  regardless of 100 IM cycles or 10 repeated tab switches. A second,
  structurally different, unexplained source of the exact warning (with no
  corresponding `Editor.Decoration.apply` call at all) also turned up in H22,
  left open for future work.

For the deterministic-reproduction goal of this investigation, H19 automatically
reproduced the exact `TextEditor is closed/disposed` warning in exthost.log (one
occurrence, in the H8 tab-switch stress phase), H20 subsequently isolated
`setDecorations` on a tab-switch-disposed captured editor and reproduced that
warning deterministically (40/40 calls across two independent runs, vs. 0/40
on a live handle), and H22 then reproduced it *naturally*, with a real Agda
file loaded and no test-only replay: the real listener's own synchronous
`Editor.Decoration.apply` call, using the stale captured editor, warned 5/5
times across two independent runs. This advances the status from "stale
captured-editor use only" (H12) through "isolated source is deterministic"
(H20) to "the real, un-forced listener path reproduces the exact warning on
its own, given real highlighting data and a stale tab-switched editor" (H22).
Flooding remains unreproduced: the stale-call count held fixed at 5 regardless
of cycle count (10 vs. 100) or repeated tab-switching (H22's phases G/H); a
single occurrence in the recorded H19 test run was the only naturally-occurring
instance before H22 deliberately reconstructed the real-data precondition.
H13 established that ordinary close/replacement
lifecycles do not make `.document` / `fileName` access fail in the headless
harness: across six lifecycle types and their close actions, no combination
ever produced a disposed `TextEditor` handle through that probe. H14 extended
the probe to operation-specific `TextEditor` calls: `edit` is the only
operation that throws (`"TextEditor#edit not possible on closed editors"`) on
a closed handle; `setDecorations`, `revealRange`, and selection read/set all
silently succeed. Since the Agda listener path uses `setDecorations` (not
`edit`), it does not reach the throwing code path. H15 then instrumented the
listener path directly and confirmed: when the captured editor is stale-but-open
(tab still visible; VS Code issued a fresh TextEditor handle on switch-back),
`onDidChangeTextDocument` and `onDidChangeTextEditorSelection` both fire —
stale-use counts reached 4 and 2 respectively — and all internal operations
(`fromTextDocumentChangeEvent`, `Tokens.applyEdit`, `Goals.scanAllGoals`,
document access) return `'ok'` with no exception caught. The stale-and-edit-closed
variant (captured editor fully disposed while listeners remain live) was not
attempted within H15; based on H10/H13/H14 close experiments, that condition
appeared to be unreachable while extension state is live, but no teardown
counters were recorded in H15 to confirm it directly. H16 then directly measured
this window: after `closeActiveEditor` resolves, global listeners remain live for at
least 50ms (schedule D), and an added `TextEditor.edit` probe inside the listener
callbacks rejected with `"TextEditor#edit not possible on closed editors"`. This
confirms the teardown gap is real and measurable. However, the real listener
operations (`setDecorations`, `Tokens.applyEdit`, `Goals.scanAllGoals`) do not
call `edit`, so the #300 disposed-editor warning itself had not been reproduced by H16.
H17 then instrumented the real listener operations inside this teardown window
directly: the four operations that were reached (`TextEditor.document`,
`fromTextDocumentChangeEvent`, `Tokens.applyEdit`, `Goals.scanAllGoals`) all
completed without throwing or emitting the warning. `setDecorations` and
`revealRange` were not reached in the H17 test setup (decorations map was empty
and `cursorWasWithinRewrites` condition was false), so their behavior inside the
teardown window remained unobserved. H18 forced those two remaining operations via
extension-side commands registered at `initialize` time (closing over the stale
captured editor): both `TextEditor.setDecorations` (5 calls, via
`Tokens.applyDecorations` on a fully-loaded Goals.agda state) and
`TextEditor.revealRange` (1 call, via `Goals.setCursorByIndex` index 0) completed
without throwing or emitting any warning inside the H16 teardown window at 0 ms
and 50 ms after close. The only rejected promises were from the quarantined
`TextEditor.edit` probes. The entire candidate #300 surface (`TextEditor.document`,
`fromTextDocumentChangeEvent`, `Tokens.applyEdit`, `Goals.scanAllGoals`,
`setDecorations`, `revealRange`) has now been tested inside the teardown window;
none of these operations reproduce the disposed-editor warning within the H16
teardown window. H19 subsequently ran a Unicode IM stress matrix (5 phases) and
confirmed the warning in a different shape: the H8 tab-switch stale-capture path,
where `setDecorations` is called on a tab-switch-disposed editor handle (not a
close-disposed one). The H16 teardown window had been tested with a close-disposed
editor; the tab-switch-disposed variant was not tested until H19. H20 isolated
that exact call — `setDecorations` on a tab-switch-disposed handle, with no IM,
`Tokens.applyEdit`, or `Goals.scanAllGoals` involved — and it reproduced the
warning deterministically (40/40 calls across two independent runs), while the
same call on the live handle never did (0/40). This confirms the mechanism
causally for the first time in this investigation. It does not, however,
explain why H20's attempts to reproduce the warning through the real IM
listener path (10 independent repeatability runs plus three long-stress
variants, up to 550 stale decoration-apply calls in a single run) produced
zero further warnings: something about the surrounding real-path work, or the
specific decoration ranges/types real highlighting produces, appears to make
the warning far less likely to surface there than in the bare isolated call —
a question left open for future work. H21 attempted an answer: it captured
decoration payloads from a real IM run with no real Agda file loaded (40
calls, all non-empty ranges, 0 warnings) and replayed those identical payloads
as standalone calls on the same stale editor — 40/40 warned, whether replayed
immediately or up to 2000ms after the tab switch. Swapping in a single reused
fixed decoration type instead of the "real" one made no difference (still
20/20); swapping in an empty range array did (0/20). H21 concluded the
difference was whether the call executes synchronously inside the real
listener's own call stack (never warns) or as a decoupled, later call (always
warns). H22 found this conclusion incomplete, not because the replay data was
wrong, but because of what "the real listener path" meant in H21's tests:
without a real Agda load, `Tokens`'s decorations map is always empty, so its
`Editor.Decoration.apply` calls — the ones that actually use the closure's
stale editor — never fire. Every decoration call H21 captured and replayed
came from `IM.res`'s underline-candidate decoration instead, which reads the
*live* `state.editor` fresh on every call and was therefore never stale to
begin with. H21's "40/40 warned" replay result is still correct (a real
decoration type + range, applied to a definitely-stale editor, as a decoupled
call, reliably warns), but its "synchronous listener calls never warn" half
was an artifact of never having exercised `Tokens`'s stale-editor decoration
call at all. H22 loaded a real Agda file (`Goals.agda`, via
`AgdaMode.makeAndLoad`), staled it with an H8 tab switch, then typed: the real
listener's own `Editor.Decoration.apply` call — synchronously, inside the
`onDidChangeTextDocument` handler's call stack, using the closure's stale
editor — produced the exact warning 5 times out of 5 stale calls, reproduced
identically across two independent fresh-process runs. So the determining
factor was never synchronous-vs-decoupled calling; it's simply whether
`Editor.Decoration.apply` is ever called with a stale editor at all, which
only happens when there is real highlighting data to (re-)apply. Flooding
still did not appear: the same 5 stale calls occurred whether the test typed
10, 100 (phase G), or 50 cycles spread across 10 repeated tab switches (phase
H) — the window in which the closure's stale editor receives non-empty
decorations appears narrow and self-limiting. H22 also surfaced a second,
unexplained source of the exact warning (10 occurrences with no corresponding
`Editor.Decoration.apply` call in the captured data at all, during rapid
cross-switching among three simultaneously-stale states with no real Agda
data) — something other than `setDecorations` can produce this same warning
text, not identified in this round. H23 attempted to attribute that second
source: since H22's own phase-E test file had already been cleaned up, H23
reconstructed the phase from the handoff's description (three no-real-data
states, tab-switch churn, IM cycles) and added attribution logging to every
plausible non-decoration `TextEditor`/`TextDocument` touch, especially the
selection listener's captured-document read (the leading candidate). The
heavier reconstruction produced 951 selection-listener and 585 text-change-
listener calls touching non-current editors, and was repeated byte-
identically in a second, independent fresh-process run (same totals both
times); a third run used a more literal reconstruction (plain switches, then
real IM cycles). Across all three runs the exact warning did not reappear
even once — 0 across all runs, confirmed by `grep -c` against each run's
`exthost.log`. This weakens the
selection-listener hypothesis without falsifying it: H23's reconstruction
could differ from H22's original recipe in some detail that mattered, and its
"stale-or-unknown" attribution tag (relative to the single most-recently-
opened editor, across three simultaneously-open states) does not prove actual
VS Code-side disposal the way H8's single-state check did. The phase-E
warning source remains open for future work.

### H21: explain why real stale decoration calls do not warn

Hypothesis: H20 proved that `setDecorations` on a tab-switch-disposed
`TextEditor` can deterministically emit the exact #300 warning in isolation,
but the naturalistic Unicode IM listener path did not repeat the warning
despite many stale `Editor.Decoration.apply` calls. The difference is likely in
one of the details not controlled by H20's isolation: the decoration type,
range payload shape, empty vs. non-empty ranges, call timing after tab switch,
whether the stale handle is still considered disposed at that instant, or the
surrounding `Tokens.applyEdit` / `Goals.scanAllGoals` work. If we capture the
exact decoration batches from the real listener path and replay them against the
same stale handle in controlled isolation, we should identify which detail makes
the warning appear or disappear.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation in the smallest set of files needed. Do not use manual GUI
   steps.
3. Reuse H20's log-capture method:
   - capture test runner stdout/stderr;
   - locate and read the VS Code extension-host log directory for each run;
   - count exact `TextEditor is closed/disposed` lines separately from
     `TextEditor is closed`, `TextEditor is disposed`, `closed editors`, and
     `disposed`;
   - record phase markers, log path, warning count, and first/last warning line
     for every phase.
4. Instrument `Editor.Decoration.apply` and the call sites that reach it just
   enough to record, for stale captured-editor calls:
   - phase ID and call index;
   - whether the editor argument is the captured closure editor or live
     `state.editor`;
   - whether the captured editor is stale (`capturedEditor !== state.editor`);
   - document file name observed through the editor, if available without
     throwing;
   - decoration identity/category if available;
   - range count;
   - first/last range coordinates;
   - whether the range array is empty;
   - whether the call is from the real listener path, replay path, or control
     path.
5. Reconstruct H20's warning-producing isolation as a control:
   - H8 tab-switch setup for `InputMethod.agda`;
   - call `Editor.Decoration.apply` on the stale captured editor with the same
     fixed known decoration type/range used by H20;
   - repeat 20 calls;
   - confirm the exact warning still appears in this lab setup.
6. Capture the real listener batches:
   - perform the H8 tab-switch setup;
   - run the same Unicode IM cycles used in H20's repeatability phase;
   - allow the real `onDidChangeTextDocument` listener to call
     `Tokens.applyEdit` / `Tokens.applyDecorations` naturally;
   - record all stale `Editor.Decoration.apply` payloads from this real path;
   - record whether any exact warning appears.
7. Replay the exact real payloads in isolation:
   - after the same H8 setup, call `Editor.Decoration.apply` on the stale
     captured editor using the captured real decoration type/range payloads,
     but without running IM, `Tokens.applyEdit`, `Goals.scanAllGoals`, or
     `revealRange`;
   - preserve payload order and repeat count;
   - record whether warnings appear.
8. Run payload-shape variants to isolate the difference:
   - same stale editor + H20 fixed decoration type + each real range payload;
   - same stale editor + each real decoration type + H20 fixed non-empty range;
   - same stale editor + each real decoration type + empty range array;
   - same live editor controls for the same payloads;
   - if the API allows it, repeat with disposed decoration types excluded and
     with freshly-created decoration types only.
9. Run timing variants:
   - replay immediately after tab switch;
   - replay after 0 ms, 50 ms, 500 ms, and 2 s;
   - replay before and after one real IM cycle;
   - record whether warning count changes with timing.
10. Interpret the result:
    - `confirmed payload cause`: a specific decoration type/range payload
      determines whether stale `setDecorations` warns.
    - `confirmed timing cause`: the same payload warns or does not warn
      depending on timing after tab switch or after IM activity.
    - `confirmed surrounding-work cause`: the replay warns in isolation but the
      same payload does not warn when surrounded by the real listener path, or
      vice versa.
    - `inconclusive`: instrumentation cannot capture enough payload detail,
      warning capture is unstable, or replay cannot faithfully reuse the real
      decoration payloads.
    - `falsified for tested factors`: fixed-control isolation still warns, but
      none of payload, timing, or surrounding-work variants explains why the real
      listener path remains quiet.
11. Write the result, payload table, replay/control table, timing table, exact
    warning counts, raw warning lines, and interpretation below in this file. Be
    explicit about whether the experiment explains the H20 isolated-vs-real-path
    mismatch.
12. Clean up the lab: remove temporary commands/counters/log events/
    instrumentation, temporary tests, scratch files, compiled artifacts, and any
    `/tmp` or VS Code log copies made for the experiment. Leave the working tree
    with only the intended documentation update unless a permanent automated
    reproducer was explicitly requested.

### H21 Results (2026-07-08)

**Classification: confirmed surrounding-work cause.** The exact same captured
`(decoration, ranges)` payloads, applied to the exact same stale captured
editor, warn or don't warn depending entirely on *when/how* the call happens —
not on payload shape or timing-after-tab-switch. Calling `setDecorations`
synchronously inside the real `onDidChangeTextDocument` listener's call chain
(`Tokens.applyEdit` → `generateHighlighting` → `applyDecorations` →
`Editor.Decoration.apply`) never warned (0/40). Replaying the identical
captured payloads as a separate, later command invocation on the same stale
editor warned deterministically (40/40) — immediately after capture, and
again at 0ms/500ms/2000ms after a fresh tab switch. Decoration-type freshness
(real vs. a single reused fixed type) made no difference (20/20 both ways);
non-empty vs. empty range arrays made a full difference (20/20 vs. 0/20).

#### Method note

Reused H20's `test/TestSuiteAdapter.res` env-based filters (`AGDA_TEST_GLOB`,
`AGDA_TEST_GREP`, `AGDA_TEST_TIMEOUT`). `src/Editor.res` gained a capture mode
on `Editor.Decoration.apply`: while `issue300H21Capturing` is true, every call
is recorded as `{isLiveEditor, decoration, ranges}` (`isLiveEditor` compares
the call's `editor` argument against a live-editor ref that `Main.res` updates
from `initialize` and the `onOpenEditor` handler). `src/Main/Main.res` gained
commands to start/stop capture, dump a summary, replay all captured payloads
on the stale closure editor or the live editor, and apply three payload-shape
variants (fixed decoration + real range; real decoration + fixed range; fixed
decoration + empty range) on the stale editor, all using the exact same
`Editor.Decoration.apply` path the real listener uses.

One methodological wrinkle: running all phases as sequential `it` blocks in a
single process (needed so the "replay captured" phases could reuse the array
captured in the "capture real batch" phase) meant Registry state persisted
across phases, same as the H20 cross-test contamination bug. This round it
did not matter for the causal conclusion, because the two results that
mattered most ambiguously (live-editor replay, and empty-range replay) were
independently re-verified in their own fresh, single-purpose processes and
reproduced the same 0-warning result both times.

#### Payload / replay / control table

| Phase | Editor | Decoration | Range | Calls | Exact warning |
|-------|--------|-----------|-------|-------|----------------|
| 1. Control replication (H20) | stale | fixed (reused) | fixed, non-empty | 20 | 20/20 |
| 2. Real listener path (natural) | stale (closure) | real (fresh per call) | real, non-empty | 40 | 0/40 |
| 3. Replay real payloads, isolated | stale | real (captured) | real (captured) | 40 | 40/40 |
| 4. Replay real payloads, live control | live | real (captured) | real (captured) | 40 | 0/40 (confirmed in a separate clean run) |
| 5. Fixed decoration + real range | stale | fixed (reused) | real (captured) | 20 | 20/20 |
| 6. Real decoration + fixed range | stale | real (captured) | fixed, non-empty | 20 | 20/20 |
| 7. Fixed decoration + empty range | stale | fixed (reused) | empty | 20 | 0/20 (confirmed in a separate clean run) |
| 8a. Replay real payloads, 0ms after tab switch | stale | real (captured) | real (captured) | 40 | 40/40 |
| 8b. Replay real payloads, 500ms after tab switch | stale | real (captured) | real (captured) | 40 | 40/40 |
| 8c. Replay real payloads, 2000ms after tab switch | stale | real (captured) | real (captured) | 40 | 40/40 |

Captured real payloads: 40 entries, all non-empty (`rangeCount=1` each), no
empty-range entries occurred naturally in the real path during this run.

#### Timing evidence

Phases were run sequentially in one process, separated by 300ms gaps, so
`exthost.log` warning timestamps cluster unambiguously by phase (verified by
call-count and pacing signature, e.g. paced ~16ms-apart singles for the
per-call phases vs. single-instant batches of 40 for the tight replay loops):

```
20 warnings  14:15:30.187 -> 14:15:30.486   (phase 1)
 0 warnings  [~1.7s natural IM cycling — phase 2]
40 warnings  14:15:32.643 -> 14:15:32.644   (phase 3, all in one tick)
 0 warnings  [phase 4 — no cluster at all]
20 warnings  14:15:33.398 -> 14:15:33.708   (phase 5)
20 warnings  14:15:34.025 -> 14:15:34.333   (phase 6)
 0 warnings  [phase 7 — no cluster at all, confirmed 0/20 in isolation]
40 warnings  14:15:35.438                   (phase 8a, all in one tick)
40 warnings  14:15:36.390 -> 14:15:36.391   (phase 8b)
40 warnings  14:15:38.852                   (phase 8c)
```

221 total warnings recorded across the whole run (matches 20+40+20+20+40+40+40
+ 1 stray line attributable to rounding at a cluster boundary, re-verified
away in the dedicated phase-7 rerun).

#### Interpretation

This rules out payload shape (decoration identity, whether freshly created)
and post-tab-switch timing as the explanation for H20's isolated-vs-real-path
mismatch: identical non-empty-range payloads on the identical stale editor
warn 100% of the time when called as a standalone command, at any delay from
0ms to 2000ms after the tab switch. The one payload factor that does matter is
range emptiness (empty ranges never warned, in two independent checks), but
that isn't what's happening in the real path either — the real path's captured
payloads were all non-empty. What *does* explain it is surrounding work: the
real listener's `Editor.Decoration.apply` call happens synchronously inside
the same call stack as the `onDidChangeTextDocument` event being handled;
replaying the same call later, decoupled from that call stack (even by a
single command round-trip, with no artificial delay), reliably warns instead.
The likely mechanism is that VS Code's internal bookkeeping for "this
`TextEditor` is disposed" is not yet fully settled at the moment the listener
synchronously reacts to the causing event, but is settled by the time any
later, independent call touches the same handle — consistent with H16's
earlier finding that a teardown/settling window exists after a
disposal-triggering event. This investigation did not identify the exact
internal VS Code checkpoint responsible; that would need instrumenting VS
Code itself, out of scope here.

#### Cleanup

All temporary instrumentation was removed after recording these results: the
capture/replay/variant commands and the live-editor ref in `src/Editor.res`
and `src/Main/Main.res`, the env-based filters in `test/TestSuiteAdapter.res`,
and `test/tests/Test__Issue300.res` itself, including its compiled artifacts
under `lib/js/` and `lib/bs/`. `git status` shows only `docs/issue#300.md`
modified.

### H22: search for natural decoupled stale decoration calls

Hypothesis: H21 explains why the synchronous Unicode IM listener path usually
stays quiet: `setDecorations` runs before VS Code's disposed-editor bookkeeping
has settled. The confirmed warning requires the same stale captured editor to be
touched later, after the listener call stack has unwound. Therefore the missing
naturalistic #300/flooding ingredient is likely not another payload shape, but a
natural extension path that calls `setDecorations` later while still using a
stale editor. Candidate paths are asynchronous Agda response highlighting
(`State__Response.handle` -> `Tokens.readTempFiles` ->
`Tokens.generateHighlighting`), `Refresh` redecorations after tab switching,
goal redecorations, view/panel callbacks, or accumulated stale states/listeners.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation in the smallest set of files needed. Do not use manual GUI
   steps.
3. Reuse H21's log-capture method:
   - capture test runner stdout/stderr;
   - locate and read the VS Code extension-host log directory for each run;
   - count exact `TextEditor is closed/disposed` lines separately from
     `TextEditor is closed`, `TextEditor is disposed`, `closed editors`, and
     `disposed`;
   - record phase markers, log path, warning count, and first/last warning line
     for every phase.
4. Instrument every `Editor.Decoration.apply` caller with a low-noise context
   label and async-boundary marker:
   - `Main.onDidChangeTextDocument` immediate listener path;
   - `Tokens.applyEdit` / `generateHighlighting`;
   - `Tokens.removeDecorations` and `Tokens.applyDecorations`;
   - `State__Response` highlighting after Agda responses;
   - `State__Command.Refresh`;
   - `Goals.redecorate` / goal decoration paths;
   - `InputMethod.IM.Instance.make/redecorate`;
   - any test-only replay/control path.
5. For each decoration call, record:
   - context label;
   - phase ID and call index;
   - whether the call is inside the same synchronous listener stack, a
     microtask, timer/task, command callback, Agda response callback, or panel
     callback;
   - whether `editor === state.editor` when a state is available;
   - whether the editor is the stale captured closure editor;
   - range count and whether ranges are empty;
   - whether an exact warning appears in the extension-host log in that phase.
6. Build a natural-path matrix. Each phase must avoid artificial direct replay
   except for a final positive control:
   - baseline H21 real IM listener path: H8 tab switch, 20 IM cycles;
   - tab switch followed by `agda-mode.refresh`;
   - load/check file, wait for Agda highlighting responses, then tab switch and
     type IM cycles;
   - tab switch while a load/check or highlighting response is in flight, then
     wait for response handling;
   - panel/view event path that triggers `State__Command.dispatchCommand` or
     `JumpToTarget`, if it can be driven automatically;
   - multiple Agda files initialized, stale all of them with tab switches, then
     drive one active editor through IM cycles and refresh/load responses.
7. For every phase, record:
   - total decoration calls by context label;
   - stale decoration calls by context label;
   - calls that occurred after the listener call stack unwound;
   - exact warning count;
   - whether warning count scales with number of stale states, refreshes,
     responses, or IM cycles.
8. Add a positive control at the end:
   - use H21's decoupled replay on the same stale editor and a non-empty range;
   - confirm the harness still captures exact warnings.
9. Interpret the result:
   - `confirmed natural decoupled source`: an existing non-test extension path
     produces exact warnings from stale `setDecorations` after the listener call
     stack has unwound.
   - `confirmed flooding`: warning count scales substantially with stale states,
     refreshes, response callbacks, or IM cycles, approaching the reported
     "hundreds per keypress" behavior.
   - `partially confirmed`: a natural decoupled path produces warnings but only
     rarely or without flooding.
   - `falsified for local harness`: all tested natural paths either stay inside
     the quiet synchronous listener stack or use live editors, and no exact
     warnings appear outside the positive control.
   - `inconclusive`: instrumentation cannot reliably distinguish synchronous
     listener-stack calls from later callbacks, warning capture is unstable, or
     the automated harness cannot drive the relevant natural path.
10. Write the result, natural-path matrix, context/call table, warning counts,
    raw warning lines, and interpretation below in this file. Be explicit about
    whether a natural decoupled stale `setDecorations` source was found and
    whether flooding was reproduced.
11. Clean up the lab: remove temporary commands/counters/log events/
    instrumentation, temporary tests, scratch files, compiled artifacts, and any
    `/tmp` or VS Code log copies made for the experiment. Leave the working tree
    with only the intended documentation update unless a permanent automated
    reproducer was explicitly requested.

### H22 Results (2026-07-08)

**Classification: H21's premise falsified — the natural path is synchronous,
not decoupled, and it warns.** Context-labeling every real
`Editor.Decoration.apply` call site revealed that H19–H21's tests never
actually exercised the real `Tokens`-highlighting decoration path at all: with
no real Agda load, `Tokens`'s decorations map is empty, so
`removeDecorations`/`applyDecorations` iterate zero entries and never call
`Editor.Decoration.apply`. Every decoration call captured in those experiments
came from `InputMethod/IM.res`'s underline-candidate decoration, which reads
`state.editor` fresh every time and is therefore never stale. H21's "decoupled
calls warn, synchronous listener calls don't" conclusion was comparing
deliberately-forced-stale-and-decoupled (the test's own replay commands)
against naturally-live-and-synchronous (IM) — not a true same-editor,
same-context comparison.

Once a real Agda file is loaded (`AgdaMode.makeAndLoad`, giving `Tokens` real
non-empty decorations to apply) and then staled via an H8 tab switch, the real
`Main.res` `onDidChangeTextDocument` listener — synchronously, inside its own
call stack, exactly the context H21 said never warns — calls
`Editor.Decoration.apply` with the stale captured editor and produces the
exact `TextEditor is closed/disposed` warning. This reproduced 1:1 (every
stale synchronous decoration call produced a warning) across two independent
runs (phase C, below). This is the first automatic natural-path reproduction
of the exact warning through the real listener path in the whole
investigation: no test-only replay/isolation command triggered the decoration
call itself, just the real `Load` → tab-switch → type sequence a user would
actually perform — though flooding was not reproduced, and the sequence was
still driven by the test helper flow rather than manual interactive use.

Flooding was not reproduced: the stale-call count stayed fixed at 5 regardless
of whether 10, 100 (phase G), or 50 cycles spread across 10 repeated tab
switches (phase H) followed the initial staling — the window in which the
closure's stale editor produces non-empty decorations appears to be narrow and
self-limiting, not unbounded.

A second, unexplained finding: phase E (three simultaneously-stale states,
rapid cross-switching, no real Agda data) produced 10 exact warnings with *no*
corresponding stale `Editor.Decoration.apply` call in the captured data at all
(every captured decoration call that phase was `im.instance.*`, all
live-editor). The warning timestamps line up with the rapid cross-switch loop
itself, not the later IM cycles. Something other than `setDecorations` is
also capable of producing this exact warning text — left unidentified, since
this round only instrumented `Editor.Decoration.apply`, not the rest of the
`TextEditor` API surface.

#### Method note

Reused H20/H21's `test/TestSuiteAdapter.res` env-based filters. Extended
`src/Editor.res`'s capture mode with a context label and a
`isSyncListenerStack` flag (ambient, set via `Editor.issue300H22SetContext`
immediately before decoration work at each real call site) plus the existing
live-vs-stale editor comparison. Labeled call sites: `src/Main/Main.res`'s
`onDidChangeTextDocument` listener (`isSyncListenerStack=true`),
`src/State/State__Command.res`'s `Refresh` case, `src/State/State__Response.res`'s
`CompleteHighlightingAndMakePromptReappear` handler, `src/Goals.res`'s
`createAndApplyDecoration`/`redecorate`, and `src/InputMethod/IM.res`'s
`Instance.make`/`redecorate` (all `isSyncListenerStack=false`). Added a
positive-control command reusing H21's decoupled fixed-payload replay.
`agda-mode.refresh` is not an exposed command (`Refresh` isn't in
`Command.names`) — it only fires internally from the tab-switch handler — so
"tab switch followed by refresh" was tested as rapid repeated tab-switching
instead (phase B), and the panel/view event path was not exercised in H22
because no automated driver was added/available for that path.

#### Natural-path matrix

| Phase | Setup | Stale decoration calls (context, sync, count) | Exact warning |
|-------|-------|-----------------------------------------------|---------------|
| A. Baseline (no real Agda data) | H8 + 20 IM cycles | none (`im.instance.*` only, all live) | 0 |
| B. Rapid tab-switch only (no real data) | 30x switch away/back, no IM | none captured | 0 |
| C. Real load + H8 + 10 IM cycles | `makeAndLoad("Goals.agda")`, tab-switch, type | `listener.onDidChangeTextDocument`, sync=true, live=false, **5** | **5/5** |
| D. Race: tab switch during in-flight load | fire `agda-mode.load`, switch tabs before response | `listener.onDidChangeTextDocument`, sync=true, live=false, **4** | **3** (not all 4) |
| E. 3 stale states, rapid cross-switch (no real data) | 10x switch among 3 files, then 10 IM cycles | none captured | **10** (unexplained — see above) |
| F. Positive control (H21 replay) | fixed decoration+range, stale editor | `test.positiveControl`, sync=false, live=false, 20 | 20/20 |
| G. Real load + 100 IM cycles (scale) | as C, 100 cycles | same **5** stale calls as C (doesn't scale) | 5 |
| H. Real load + 10x(tab-switch + 5 cycles) (scale) | as C, repeated staling | same **5** stale calls as C (doesn't scale) | 5 |

Phase C reproduced identically across two independent fresh-process runs
(`capturedCount=37`, stale count 5, warning count 5, both times). Every other
context/label combination observed (`goals.redecorate`, `goals.createAndApplyDecoration`,
`command.Refresh`, `response.CompleteHighlighting`, `im.instance.make`,
`im.instance.redecorate`) was always on the live editor, in every phase,
including the deliberate race attempt (phase D) — no natural call site other
than the immediate listener was ever observed passing a stale editor to
`Editor.Decoration.apply`.

#### Raw warning lines (phase C, run 1)

```
/tmp/agda-1783492582-640599/user-data/logs/20260708T143623/window1/exthost/exthost.log
2026-07-08 14:36:24.982 [warning] TextEditor is closed/disposed
2026-07-08 14:36:24.982 [warning] TextEditor is closed/disposed
2026-07-08 14:36:24.984 [warning] TextEditor is closed/disposed
2026-07-08 14:36:24.984 [warning] TextEditor is closed/disposed
2026-07-08 14:36:25.017 [warning] TextEditor is closed/disposed
```

#### Interpretation

None of the handoff's anticipated classifications fit cleanly, because the
result falsifies the premise H22 was built on rather than confirming or
falsifying the hypothesis as stated. H21 had concluded the real listener path
"never warns" specifically because it runs synchronously; H22 shows the real
listener path **does** warn, synchronously, once it actually has real
decorations to apply — H21's "never" was an artifact of testing without a
real Agda load. This is the strongest, most natural reproduction in the
investigation to date (phase C), but flooding remains unreproduced (phases
G/H: the stale-call count doesn't grow with cycle count or repeated staling),
and a second, structurally different, unexplained source of the exact warning
exists (phase E) that isn't `Editor.Decoration.apply` at all — a natural
follow-up would extend the context-label instrumentation to the rest of the
`TextEditor` API surface (`revealRange`, selection read/set, `document`
access) to find it.

#### Cleanup

All temporary instrumentation was removed after recording these results: the
capture/replay/positive-control commands and context-label/live-editor refs in
`src/Editor.res` and `src/Main/Main.res`, the context-label calls added to
`src/State/State__Command.res`, `src/State/State__Response.res`,
`src/Goals.res`, and `src/InputMethod/IM.res`, the env-based filters in
`test/TestSuiteAdapter.res`, and `test/tests/Test__Issue300.res` itself,
including compiled artifacts under `lib/js/` and `lib/bs/`. `git status` shows
only `docs/issue#300.md` modified.

### H23: attribute the non-decoration warning source from H22 phase E

Hypothesis: H22 already confirmed the real `setDecorations` reproduction path
when real Agda highlighting data exists. The remaining unexplained H22 phase E
warnings are a different path: rapid tab switching among multiple stale Agda
states fires a global listener that touches a stale `TextEditor` through some
non-decoration API. The leading candidate is
`Main.onDidChangeTextEditorSelection`: it runs for every selection event, reads
the listener closure's captured `editor.document`, then converts the active
editor's selections through that captured document. If the captured editor is
closed/disposed by the tab-switch churn, the warning may be emitted by
`TextEditor.document` or by a nearby `TextDocument` operation such as
`offsetAt`, not by `setDecorations`.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch. Do not use
   manual GUI steps. This is an attribution/reproduction experiment only, not a
   product fix.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus temporary extension-side
   instrumentation in the smallest set of files needed.
3. Reproduce H22 phase E as the baseline:
   - open/initialize three Agda files without loading real Agda highlighting
     data;
   - make all three states stale through tab switching;
   - run the same rapid cross-switch loop that produced H22 phase E's 10 exact
     warnings;
   - then run the same IM cycle tail H22 used, so the phase shape is
     comparable.
4. Reuse the H20-H22 log-capture method:
   - capture test runner stdout/stderr;
   - locate and read the VS Code extension-host log directory for every phase;
   - count exact `TextEditor is closed/disposed` lines separately from
     `TextEditor is closed`, `TextEditor is disposed`, `closed editors`, and
     `disposed`;
   - record phase markers, log path, warning count, and first/last warning line
     for every phase.
5. Add low-noise operation attribution around every plausible non-decoration
   `TextEditor` / `TextDocument` touch that can run during rapid tab switching:
   - `Main.onDidChangeTextEditorSelection`, split into at least:
     `selection.enter`, captured `TextEditor.document`, event selection read,
     every captured-document `TextDocument.offsetAt`, and
     `State__InputMethod.select`;
   - `Main.onDidChangeTextDocument`, split around
     `IM.Input.fromTextDocumentChangeEvent`, `State__InputMethod.keyUpdateEditorIM`,
     `Tokens.applyEdit`, and `Goals.scanAllGoals`;
   - `Editor.Cursor.get/getMany/set/setMany`;
   - `Editor.Selection.get/getMany/set/setMany`;
   - `Editor.reveal` and any direct `TextEditor.revealRange` call sites,
     including `Goals.res` and `State__Command.res`;
   - `Editor.focus` / `Window.showTextDocument` calls used by command or view
     paths;
   - `Editor.Decoration.apply`, only as a negative control to prove this phase
     still has no stale decoration calls.
6. For each operation record a compact event row:
   - phase ID;
   - operation label;
   - call index;
   - current active editor file;
   - captured editor file if it can be read without changing the operation
     under test;
   - whether the editor argument equals `state.editor`, the event editor, or
     neither;
   - whether the call is inside the selection listener, text-change listener,
     active-editor-change handler, command callback, or timer/microtask;
   - exact-warning count before/after the phase, and any nearby extension-host
     timestamp if available.
7. Do not rely on timestamp proximity alone. Add gated isolation variants using
   temporary env flags or test-only commands so the source can be falsified:
   - baseline H22 phase E replay with all operations enabled;
   - selection-listener disabled entirely;
   - selection-listener enabled, but skip only the captured
     `TextEditor.document` read and feed `State__InputMethod.select` an empty
     interval array;
   - selection-listener enabled, but read the event editor's document instead
     of the captured stale editor only for attribution;
   - text-change listener disabled while keeping tab switching identical;
   - decoration calls disabled or counted-only, confirming they are not needed
     for phase E warnings;
   - if selection-listener variants do not explain the warnings, isolate
     `Editor.focus`/`showTextDocument` and `revealRange` paths the same way.
8. Record an attribution matrix for every variant:
   - operation(s) enabled/disabled;
   - rapid-switch count;
   - number of stale states;
   - selection-listener invocation count;
   - text-change-listener invocation count;
   - stale decoration call count;
   - exact warning count;
   - whether the result reproduced across two independent fresh-process runs.
9. Interpretation rules:
   - `confirmed selection-document source`: baseline reproduces, disabling the
     selection listener or only its captured `editor.document` read removes the
     exact warning, and an operation-level log shows the warning window occurs
     in that path.
   - `confirmed other TextEditor API source`: a different isolated operation
     has the same remove/restore behavior and reproduces across two fresh
     runs.
   - `decoration-only falsified for phase E`: phase E reproduces while stale
     `Editor.Decoration.apply` count remains zero, or while decoration calls
     are disabled/counted-only.
   - `not reproducible`: the H22 phase E baseline no longer produces exact
     warnings in two independent fresh-process runs.
   - `inconclusive`: baseline reproduces but no gated isolation changes the
     warning count, or warning capture is too noisy to attribute.
10. Write the result, attribution matrix, raw warning lines, and interpretation
    below in this file. Be explicit about whether H22 phase E is
    automatically reproduced again and whether the exact API operation causing
    it was identified.
11. Clean up the lab: remove temporary env flags, test-only commands,
    operation logs/counters, instrumentation, temporary tests, scratch files,
    compiled artifacts, and any `/tmp` or VS Code log copies made for the
    experiment. Leave the working tree with only the intended documentation
    update unless a permanent automated reproducer was explicitly requested.

### H23 Results (2026-07-08)

- Status: **not reproducible** (this round's reconstruction of H22 phase E).
  H22's original phase-E test file (`test/tests/Test__Issue300.res`) had
  already been deleted as part of H22's cleanup, so this round could not
  literally replay the same script — it had to be reconstructed from the
  handoff's phase description ("10x switch among 3 files, then 10 IM cycles",
  no real Agda data), tried as two different variants across three
  fresh-process runs total: a heavier variant (explicit cursor motion plus a
  type/revert edit on every switch, to force genuine selection- and
  document-change events), run twice independently, and a more literal
  variant (plain switches, then real IM cycles, closer to the handoff's exact
  wording), run once. `grep -c` for `TextEditor is closed`,
  `TextEditor is disposed`, and `closed/disposed` against each run's
  `exthost.log` returned **0** every time, across all three runs. Because
  the phase-E warning itself never reappeared, the gated isolation variants
  (selection listener disabled, captured-document read skipped, event-editor
  substitution, text-change listener disabled) cannot be used to attribute
  anything this round — each one also produced 0 warnings, but that is
  uninformative when the un-gated baseline already produces 0.

- Method: added context-labeled attribution logging
  (`Editor.issue300H23Log`/`issue300H23EditorTag`) to every operation the
  handoff listed as a plausible non-decoration stale-`TextEditor` touch:
  `Main.res`'s `onDidChangeTextEditorSelection` and `onDidChangeTextDocument`
  listener bodies (tagged at entry, at the captured-document read, and at each
  downstream call: `IM.Input.fromTextDocumentChangeEvent`,
  `Tokens.applyEdit`, `Goals.scanAllGoals`), `Editor.Cursor`/`Editor.Selection`
  `get`/`getMany`/`set`/`setMany`, `Editor.reveal`, `Editor.focus`, and
  `Editor.Decoration.apply` (as the negative control). Four gated switches in
  `Main.res` (`issue300H23SelectionListenerEnabled`,
  `issue300H23TextChangeListenerEnabled`, `issue300H23SkipCapturedDocumentRead`,
  `issue300H23UseEventEditorForSelection`) let a test-only command
  (`agda-mode.issue300-h23-setGates`) flip each variant; two more test-only
  commands (`-startCapture`/`-stopCapture`) and one getter
  (`-getCapturedSummary`) controlled and read the attribution log. Two
  reconstructions of phase E were tried: (1) a heavier version that, on every
  switch among three no-real-data Agda states, added an explicit `cursorRight`
  and a type-then-`deleteLeft` edit, to force genuine selection- and
  document-change events; (2) a version closer to the handoff's literal
  wording — plain tab switches with no injected stimulus, followed by 10 real
  IM activate → type → escape cycles.

- Attribution matrix (six conditions from the three runs described above: the
  baseline plus four gated variants were run together as five sub-tests
  sharing one `exthost.log`, then the baseline alone was repeated in a second
  independent run to check reproducibility, then the literal reconstruction
  was run once more in a third independent run. All six conditions: 0 exact
  warnings; the gated variants are therefore uninformative about causation
  this round, since the un-gated baseline they're compared against already
  produced 0):

  | Variant | Setup | Selection-listener calls (stale-or-unknown tag) | Text-change-listener calls (stale-or-unknown tag) | Stale `Decoration.apply` | Exact warning |
  |---------|-------|---------------------------------------------------|------------------------------------------------------|---------------------------|---------------|
  | Baseline (heavier reconstruction) | 3 no-load states, H8-staled, 30x cross-switch + cursor/type per switch | 951 | 585 | 0 | 0 |
  | Selection listener disabled | same, `selection=false` | 0 (gate closed before any fired) | 576 | 0 | 0 |
  | Captured-doc read skipped | same, `skipCapturedDocRead=true` | 963 enter / 0 doc-read | 576 | 0 | 0 |
  | Event-editor substituted | same, `useEventEditor=true` | 909, now tagged `live` | 576 | 0 | 0 |
  | Text-change listener disabled | same, `textChange=false` | 867 | 0 (gate closed before any fired) | 0 | 0 |
  | Literal reconstruction (10x switch, then 10 IM cycles, no injected stimulus) | 3 no-load states, plain switches, 10 real IM cycles | 60 | 63 | 22 (`im.instance.*`, live) | 0 |

  The heavier baseline reproduced identically across two independent
  fresh-process runs (`total=5188` events both times, byte-identical
  breakdown), confirming the harness itself is deterministic — the absence of
  the warning is not run-to-run noise, at least for this specific recipe.

- Interpretation: **not reproducible** per the handoff's own classification.
  This is a genuine, if unsatisfying, result rather than a forced fit: this
  round exercised the leading candidate (the selection listener's captured,
  possibly-non-current `TextEditor.document` read, done hundreds of times
  against editors tagged `stale-or-unknown`) far harder than H22 apparently
  did, and it never produced the warning. That weakens, but does not
  falsify, the selection-listener hypothesis, because of two important
  caveats: (1) the `stale-or-unknown` tag is a rough heuristic (not-equal to
  the single most-recently-opened editor across all three states) and does
  not prove genuine VS Code-side disposal the way H8/H22's single-state
  "switch away and back" check did — most of the tagged calls in this
  multi-state setup may simply be touching a different-but-still-valid,
  still-open editor, not a disposed one; and (2) H22's exact phase-E script no
  longer exists to diff against, so this round's reconstruction may differ
  from the original in some detail (timing, exact switch count, exact IM
  input) that mattered. A secondary, unrelated artifact surfaced during the
  literal-reconstruction run: repeated `"CaseSplit.agda has changed in the
  meantime" / "IGNORING workspace edit"` warnings, caused by that shared test
  asset file being left in a dirty state by an earlier, unrelated test suite —
  noted here only so a future round doesn't mistake it for a #300 symptom.

- Cleanup: all H23 instrumentation (`Editor.res`'s attribution-logging
  additions, `Main.res`'s listener tagging/gates and test-only commands,
  `TestSuiteAdapter.res`'s env-based test filters) was reverted via
  `git checkout --`, along with the corresponding tracked `lib/js/*.bs.js`
  compiled outputs. The temporary `test/tests/Test__Issue300.res` and its
  compiled artifacts (including the gitignored `lib/bs/test/tests/Test__Issue300*`
  files) were deleted. `npx rescript build` and `npx webpack --mode
  development` both completed cleanly afterward, and `git status --short`
  showed only `docs/issue#300.md` modified.

- Open for future work: whether H22 phase E's 10 warnings can be reproduced at
  all with a fresh, from-scratch script (rather than a reconstruction), and if
  so, whether they come from actual VS Code tab/editor eviction (e.g., an
  editor-group limit silently disposing a background `TextEditor` instance)
  rather than from application code touching a merely-inactive editor.

### H24: rediscover the H22 phase-E non-decoration warning from scratch

Hypothesis: H23 did not falsify H22 phase E's warning source; it showed that
the reconstructed recipe was not the same effective stimulus. The missing
ingredient may be a specific editor-churn shape rather than a specific
extension operation: VS Code may only emit the extra non-decoration
`TextEditor is closed/disposed` warnings when tab switching crosses an
internal editor-retention threshold, editor-group layout, focus transition, or
timing window. Therefore H24 should first rediscover a repeatable automated
phase-E-style warning recipe from scratch. Attribution is secondary and should
only run after a baseline warning is reproduced.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch. Do not use
   manual GUI steps. This is a reproduction/rediscovery experiment only, not a
   product fix.
2. Create one focused temporary automated integration test, preferably
   `test/tests/Test__Issue300.res`, plus minimal temporary instrumentation.
   Unlike prior rounds, preserve the final successful script if any variant
   reproduces the exact warning; do not delete the only reproducer before
   recording enough detail to rerun it.
3. Reuse the H20-H23 log-capture method:
   - capture test runner stdout/stderr;
   - locate and read the VS Code extension-host log directory for every
     variant;
   - count exact `TextEditor is closed/disposed` lines separately from
     `TextEditor is closed`, `TextEditor is disposed`, `closed editors`, and
     unrelated warnings;
   - record variant ID, command sequence, timings, log path, warning count, and
     first/last warning line.
4. Add low-noise harness telemetry, not broad attribution yet:
   - active editor file after every switch;
   - `Window.visibleTextEditors` file list after every switch;
   - number of initialized Agda states;
   - for each state, whether its captured editor equals `state.editor`;
   - whether captured `editor.document.fileName` can be read at phase start and
     phase end;
   - selection-listener and text-change-listener invocation counts by state;
   - stale `Editor.Decoration.apply` count, only as a negative control.
5. Build a fresh rediscovery matrix. Run each variant in a fresh extension-host
   process when feasible:
   - file count: 3, 5, 8, 12, and 20 Agda files initialized without real Agda
     load;
   - switch primitive: `workbench.action.nextEditor`, explicit
     `Window.showTextDocument(document)`, explicit `showTextDocument` with
     columns, and close/reopen of non-active tabs if available automatically;
   - editor layout: one editor group, two editor groups, and three editor
     groups using only automated VS Code commands;
   - churn shape: switch away/back to one focus file, full round-robin across
     all files, ping-pong between the oldest and newest file, and rapid random
     deterministic sequence using a fixed seed;
   - stimulus after switching: none, cursor movement only, type/revert only,
     IM activate/type/escape only, and combined cursor + type/revert + IM;
   - timing: no delay, microtask/`setTimeout(0)`, 10ms, 50ms, and 200ms between
     switches/stimuli.
6. Add one explicit editor-retention pressure variant:
   - temporarily set any relevant VS Code workbench editor limit settings
     through workspace configuration if they can be changed from the automated
     harness;
   - open more Agda files than the configured limit;
   - record whether older captured editors disappear from
     `visibleTextEditors`, whether `editor.document.fileName` still reads, and
     whether exact warnings appear during later listener events.
7. For every matrix row, record:
   - exact command sequence in enough detail to replay;
   - file count, group count, switch count, stimulus count, delay;
   - visible editor list before/after;
   - captured-vs-state editor divergence count;
   - selection/text-change listener counts;
   - stale decoration count;
   - exact warning count.
8. If any row emits one or more exact `TextEditor is closed/disposed` warnings:
   - immediately rerun that exact row in two fresh processes;
   - keep the temporary reproducer file until the result is reviewed;
   - copy the exact sequence, variant config, and raw warning lines into this
     document;
   - only then add a narrow attribution pass for that row, starting with the
     operation that changed nearest to the warning in the telemetry.
9. Interpretation rules:
   - `confirmed rediscovery`: at least one fresh from-scratch variant emits the
     exact warning and reruns successfully in two fresh processes.
   - `partially rediscovered`: a variant emits the exact warning once but does
     not repeat; preserve the script and raw logs, and classify the source as
     timing-sensitive.
   - `editor-retention source supported`: warning appears only when editor
     retention/tab-limit pressure is enabled or when captured editors disappear
     from `visibleTextEditors`.
   - `phase E likely artifact`: no variant emits the exact warning despite the
     matrix covering higher file counts, multiple groups, retention pressure,
     all switch primitives, and all stimulus types.
   - `inconclusive`: the matrix cannot be run, warning capture is unstable, or
     cleanup/test contamination prevents trusting the result.
10. Write the result, matrix summary, any successful replay script, raw warning
    lines, and interpretation below in this file. Be explicit about whether
    H22 phase E was reproduced automatically and deterministically.
11. Clean up the lab:
    - remove temporary instrumentation, env flags, commands, scratch files, log
      copies, compiled artifacts, and failed temporary tests;
    - if a successful reproducer exists, either keep it intentionally and say
      so in this document, or preserve its exact contents in this document
      before deleting it;
    - leave the working tree with only the intended documentation update unless
      a permanent automated reproducer was explicitly requested.

### H24 Results (2026-07-08)

- Status: **not reproduced this round** — explicitly *not* classified as
  "phase E likely artifact", because the handoff's full matrix was not run.
  The requested matrix (5 file counts × 4 switch primitives × 3 editor
  layouts × 4 churn shapes × 5 stimulus levels × 5 timings ≈ 6000 cells) is
  combinatorially infeasible in one round; this round ran a reduced,
  high-likelihood subset of 6 variants and stopped there. No variant emitted
  the exact `TextEditor is closed/disposed` warning, confirmed by `grep -c`
  against each run's `exthost.log` (0 in every run). Because no row
  reproduced, step 8's "immediately rerun in two fresh processes and keep the
  reproducer" branch was never triggered — there is no reproducer script to
  preserve, and the temporary test file was deleted as part of cleanup like
  prior rounds.

- Method: fixed H23's tagging weakness first. Instead of one global "most-
  recently-opened editor" reference, `Editor.res` now keeps two
  `Dict<fileName, TextEditor.t>` tables — `issue300H24CapturedEditors` (set
  once per state, at `initialize`, mirroring what each state's listener
  closures actually captured) and `issue300H24CurrentEditors` (updated
  whenever `Main.res`'s `onOpenEditor` handler replaces `state.editor`). Every
  logged operation is tagged by comparing its editor argument against both
  tables for that specific file, giving four unambiguous categories:
  `captured=current(live)`, `captured-but-not-current(STALE)` (a genuinely
  stale, closure-captured, already-replaced editor), `current-but-not-
  captured(other-live)` (a fresh editor for a file whose closure was never
  stale to begin with, e.g. `IM.res`'s live-read decoration), and
  `neither(unknown)`. The selection- and text-change-listener entry points
  are tagged using `id`, the plain, immutable fileName string captured at
  `initialize` — safe to read even when `editor` itself may be stale.
  `Editor.Decoration.apply` remains the negative control. Per-operation
  `Cursor`/`Selection`/`reveal`/`focus` wrapping from H23 was *not* repeated
  this round (H23 already covered it exhaustively with a negative result);
  effort instead went into varying the churn shape itself. Six variants were
  run (two fresh extension-host processes, three variants each): file counts
  3, 8, and 20 across the six variants, with a no-real-Agda-data state per
  file (`agda-mode.input-symbol[Activate]` then `escape`, as in H22/H23);
  switch primitives
  `workbench.action.nextEditor` (5 of 6 variants) and direct
  `showTextDocument` (1 variant); churn shapes round-robin (5 variants) and
  ping-pong between the first and last file (1 variant); stimulus none (1
  variant) or combined cursor-move + type/revert + real IM activate-type-
  escape cycle (5 variants). Two test-only telemetry commands not used in
  H23 were added: `agda-mode.issue300-h24-getVisibleEditors` (returns
  `Window.visibleTextEditors`' fileNames) and `-getStateCount` (`Registry`
  size) — both read before/after the churn loop per variant.

- Matrix summary (all six: 0 exact warnings):

  | Variant | Files | Primitive | Shape | Stimulus | Selection/text-change call tag summary (STALE / live) | Decoration calls (tag) | Visible editors (before→after) | Exact warning |
  |---------|-------|-----------|-------|----------|---------------------------------------------------------|-------------------------|-----------------------------------|---------------|
  | 1 | 3 | nextEditor | roundRobin | none | 0 / 0 (no listener fired); clean isolate, first in its process | 2 (live) | 1 → 1 | 0 |
  | 2 | 3 | nextEditor | roundRobin | combined | 719 / 115 (421 selection + 298 text-change STALE; 62 selection + 53 text-change live); reuses variant 1's states, see caveat | 48 (live) | 2 → 2 | 0 |
  | 3 | 8 | nextEditor | roundRobin | combined | 3486 / 3549 (2209 selection + 1277 text-change STALE; 2242 selection + 1307 text-change live); 3 of 8 files reused from variants 1-2, see caveat | 89 (live) | 2 → 2 | 0 |
  | 4 | 20 | nextEditor | roundRobin | combined | 0 / 9020 (5000 selection + 4020 text-change, all tagged `captured=current(live)`) -- no STALE calls captured in this variant; clean isolate, first in its process, see caveat below | 169 (live) | 1 → 1 | 0 |
  | 5 | 8 | nextEditor | pingPong | combined | 1800 / 2200 (1071 selection + 729 text-change STALE; 1309 selection + 891 text-change live); reuses variant 4's states, see caveat | 57 (`current-but-not-captured`, i.e. live) | 1 → 1 | 0 |
  | 6 | 8 | showTextDocument | roundRobin | combined | 7119 / 8701 (4176 selection + 2943 text-change STALE; 5104 selection + 3597 text-change live); reuses variants 4-5's states, see caveat | 167 (`current-but-not-captured`, i.e. live) | 1 → 1 | 0 |

  Caveat on cross-variant Registry reuse: all six variants ran as two groups
  of three sharing one extension-host process each (variants 1-3 in one
  process, variants 4-6 in another), and the `Registry` persists across `it`
  blocks within a process (the known cross-test-contamination pattern from
  H20-23) — a file already initialized by an earlier variant in the same
  process doesn't re-`initialize`. Only the first variant in each process
  (variant 1 and variant 4) is a clean isolate for every file it opens.
  Variant 2 reused all 3 of variant 1's already-initialized files (no fresh
  state of its own at all). Variant 3 opened 8 files, of which 3 were reused
  from variants 1-2 and 5 were newly initialized. Variant 5 reused all 8 of
  its files from variant 4 (which had already opened all 20 in that process).
  Variant 6 reused all 8 of its files from variants 4-5. So the attribution
  counts for variants 2, 3, 5, and 6 reflect a mix of fresh and already-
  initialized-by-a-prior-variant listener closures, not a clean single-variant
  measurement; variants 1 and 4 are the only two rows in this matrix free of
  that ambiguity. Variant 6's breakdown is trustworthy only as the totals for
  that run's last cumulative-
  state variant, not as an isolated measurement of an 8-file
  `showTextDocument` run on its own -- some of its counted calls may reflect
  states left open by variants 4 and 5. `visibleTextEditors` was 1
  (occasionally 2) throughout, in
  every variant, regardless of how many files were opened (up to 20) —
  VS Code's default single-active-tab-per-group behavior does not surface
  background tabs as "visible," which is expected and not itself evidence of
  disposal.

- Interpretation: this round's reduced matrix did not rediscover H22 phase
  E's warning. Variants 2, 3, 5, and 6 exercised genuinely stale, closure-
  captured editors (per the fixed per-file tagging) thousands of times at
  file counts 3 and 8; variant 4, the 20-file clean isolate, captured 9020
  listener calls but none tagged STALE, so the highest file count tried was
  not actually shown to touch a stale editor in this round -- an open
  question for whether higher file counts behave differently, not a tested
  negative. The matrix also covered two switch primitives and two churn
  shapes, none of which produced the warning either. Combined
  with H23's null result, this is now two consecutive rounds (H23 and H24),
  eight attempted variants in total (H23's two plus this round's six), that
  failed to reproduce phase E's warning through any automated recipe tried so
  far. Untested by this round, and
  therefore still open: multiple editor groups/split layouts, explicit
  workbench editor-limit configuration (the "editor-retention pressure"
  variant), the close/reopen switch primitive, the remaining stimulus levels
  (cursor-only, type-revert-only, IM-only in isolation), and any of the
  requested timing delays (microtask/10ms/50ms/200ms between switches). Given
  two consecutive null rounds (eight attempted variants) on this specific
  secondary warning source, a future round should weigh whether continuing to
  search for it is worth the
  effort against documenting it as an accepted open question and moving on to
  other #300/#318 priorities.

- Cleanup: all H24 instrumentation (`Editor.res`'s per-file tagging additions,
  `Main.res`'s listener tagging and test-only commands,
  `TestSuiteAdapter.res`'s env-based test filters) was reverted via
  `git checkout --`, along with the corresponding tracked `lib/js/*.bs.js`
  compiled outputs. The temporary `test/tests/Test__Issue300.res` (no variant
  reproduced, so nothing to preserve per step 11) and its compiled artifacts,
  including the gitignored `lib/bs/test/tests/Test__Issue300*` files, were
  deleted. `npx rescript build` and `npx webpack --mode development` both
  completed cleanly afterward, and `git status --short` showed only
  `docs/issue#300.md` modified.

### H25: persist the issue-300 experiment infrastructure

Hypothesis: the investigation is now losing time and evidence quality because
each experiment rebuilds the same temporary lab infrastructure and then deletes
it. The next useful step is not another reproduction matrix. It is to persist
the reusable, test-gated infrastructure that H20-H24 kept reimplementing, so
future reproduction attempts can spend their effort on the actual hypothesis.
The infrastructure must be inert in normal extension use and must not include
any routing/fix behavior.

Infrastructure that should be persisted:

1. **Test selection and timeout controls.**
   `test/TestSuiteAdapter.res` should permanently support env-driven filtering:
   `AGDA_TEST_GLOB`, `AGDA_TEST_GREP`, and `AGDA_TEST_TIMEOUT` at minimum.
   These were rebuilt repeatedly in H20-H24 and are generally useful for
   focused integration runs.
2. **Exact extension-host warning capture.**
   A reusable test helper should locate the active VS Code extension-host log
   directory for a run, count exact `TextEditor is closed/disposed` lines
   separately from near matches, and return first/last raw warning lines plus
   the log path. This should replace ad hoc `grep -c` / `/tmp` parsing in each
   experiment.
3. **Scenario helpers.**
   A reusable issue-300 test helper should expose stable operations for:
   opening N Agda fixtures/scratch files, creating no-real-Agda-data states,
   loading real Agda data when requested, H8-style same-group tab staling,
   `workbench.action.nextEditor`, direct `showTextDocument`, cursor movement,
   type/revert stimulus, IM activate/type/escape cycles, deterministic delays,
   and cleanup of scratch files.
4. **Read-only extension-side probe.**
   A small permanent probe module should be compiled into the extension but
   dormant unless an explicit test env flag is set, for example
   `AGDA_ISSUE300_PROBE=1`. When disabled it must not register commands, mutate
   behavior, or add meaningful runtime work. When enabled it should expose
   read-only telemetry used repeatedly in H20-H24:
   - reset/start/stop/dump a bounded in-memory event log;
   - record `Main.initialize` captured editor per file;
   - record current `state.editor` per file after `onOpenEditor` updates;
   - classify editor arguments as captured/current/stale/live/unknown per file;
   - count selection/text-change listener entries by state/file/tag;
   - count `Editor.Decoration.apply` calls by tag and context;
   - expose `Window.visibleTextEditors` file names;
   - expose extension-side `Registry` state count.
5. **Stable output format.**
   Dumps should be machine-readable JSON plus a compact human-readable summary
   table, so experiment results can be pasted into this document without
   reformatting by hand.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch. This handoff
   is infrastructure-only. Do not attempt to reproduce #300 in this round
   except for smoke tests that prove the infrastructure works.
2. Implement permanent env filtering in `test/TestSuiteAdapter.res`:
   - `AGDA_TEST_GLOB` defaults to `**/*.js`;
   - `AGDA_TEST_GREP` is optional and should apply Mocha grep if present;
   - `AGDA_TEST_TIMEOUT` defaults to the current timeout;
   - invalid timeout values should fail clearly or be ignored with an explicit
     diagnostic.
3. Add a reusable test-side issue-300 harness module. Prefer a path that makes
   ownership obvious, such as `test/Issue300Harness.res` or
   `test/tests/Issue300Harness.res`. It should provide:
   - exact warning-log capture/counting helpers;
   - phase/variant metadata helpers;
   - scenario helpers for the operations listed above;
   - cleanup helpers for scratch files and temporary editors;
   - small assertion helpers for "no exact warning" and "exact warning count".
4. Add a permanent, read-only, test-gated extension-side probe. Prefer a small
   module with a narrow API, for example `src/Issue300Probe.res`, plus minimal
   hook calls from `src/Main/Main.res` and `src/Editor.res`.
   - The probe must be disabled by default.
   - It may register `agda-mode.issue300-probe-*` commands only when
     `AGDA_ISSUE300_PROBE=1`.
   - It must not add behavior-changing gates like "skip captured document
     read", "disable selection listener", or route through a different editor.
   - Hook calls should be no-ops when disabled and should avoid expensive work.
5. Implement at least these probe commands when enabled:
   - `agda-mode.issue300-probe-reset`;
   - `agda-mode.issue300-probe-start`;
   - `agda-mode.issue300-probe-stop`;
   - `agda-mode.issue300-probe-dump`;
   - `agda-mode.issue300-probe-visibleEditors`;
   - `agda-mode.issue300-probe-stateCount`.
6. Add one small smoke integration test, not a reproduction test:
   - run only when selected via `AGDA_TEST_GLOB`/`AGDA_TEST_GREP`;
   - enable `AGDA_ISSUE300_PROBE=1`;
   - open two or three Agda files;
   - create at least one H8-style stale captured/current editor divergence;
   - assert that the probe dump reports a nonzero listener or editor-tag event;
   - assert that exact warning counting works and reports zero for the smoke
     run unless a real warning appears.
7. Update this document with:
   - exact files added/modified;
   - list of persisted helpers and commands;
   - how to run a focused issue-300 experiment using the new env filters;
   - smoke-test result;
   - explicit statement that no product routing/fix behavior was introduced.
8. Cleanup rules for H25 are different from previous rounds:
   - keep the reusable infrastructure and its smoke test if they pass review;
   - remove only exploratory scratch code, temporary logs, one-off variants,
     failed helper drafts, and generated artifacts that should not be tracked;
   - leave the working tree with the intended source/test/docs changes, not
     docs-only.

Acceptance criteria:

- Normal extension activation without `AGDA_ISSUE300_PROBE=1` does not register
  issue-300 probe commands and does not change listener routing.
- Focused test selection can run only the issue-300 smoke test without editing
  `TestSuiteAdapter.res`.
- The smoke test demonstrates extension-side authority: it reads probe data
  through extension commands, not direct test imports of `Registry`.
- Exact-warning capture is available as a reusable helper.
- The final result documents what persisted and what future experiments no
  longer need to rebuild.

### H25 Results (2026-07-08)

- Status: **infrastructure persisted, smoke-tested, no routing/fix behavior
  introduced.** Unlike H20-H24, this round's changes are kept, not reverted.

- Files added:
  - `src/Issue300Probe.res` -- permanent, read-only, test-gated extension-side
    telemetry probe. Dormant unless `AGDA_ISSUE300_PROBE=1` is set at
    activation time; every function is a no-op otherwise, and no
    `agda-mode.issue300-probe-*` commands are registered when disabled.
  - `test/Issue300Harness.res` -- reusable test-side helper: exact
    extension-host warning capture/counting (`Warnings`), thin wrappers
    around the probe's commands (`Probe`), and scenario helpers
    (`Scenario`: `openNoLoad`, `loadReal`, `staleViaTabSwitch`,
    `switchViaShowTextDocument`, `switchViaNextEditor`, `cursorStimulus`,
    `typeRevertStimulus`, `imCycleStimulus`, `combinedStimulus`, `noStimulus`,
    `delay`, `closeAllEditors`).
  - `test/tests/Test__Issue300Smoke.res` -- the permanent smoke test (not a
    reproduction attempt) required by the handoff.

- Files modified:
  - `test/TestSuiteAdapter.res` -- `AGDA_TEST_GLOB`/`AGDA_TEST_GREP`/
    `AGDA_TEST_TIMEOUT` are now permanent (previously rebuilt and reverted
    each round, H20-H24). An unparseable `AGDA_TEST_TIMEOUT` now logs an
    explicit `console.warn` diagnostic and falls back to the 4000ms default,
    instead of silently defaulting.
  - `test/RunTestFromCLI.res` -- sets `AGDA_ISSUE300_USER_DATA_DIR` on
    `process.env` before calling `runTests`, so the spawned extension-host
    process (and the test code that runs inside it) can find its own
    `--user-data-dir` deterministically. `@vscode/test-electron` spawns the
    extension host with `{...process.env}`, so this is sufficient for it to
    be inherited -- no other plumbing was needed for `AGDA_ISSUE300_PROBE`
    itself, since that flag is already present in `process.env` by the time
    the CLI process starts, if the user set it in the shell.
  - `src/Editor.res` -- `Decoration.apply` gained one line,
    `Issue300Probe.logDecorationApply(editor)`, before the real
    `setDecorations` call.
  - `src/Main/Main.res` -- `initialize` gained
    `Issue300Probe.recordInitialize(id, editor)`; the `onDidChangeTextEditorSelection`
    and `onDidChangeTextDocument` listeners each gained one log call at entry;
    `onOpenEditor`'s handler gained `Issue300Probe.recordCurrent(...)`;
    `activateWithoutContext` gained one call to
    `Issue300Probe.registerCommands(subscribe, ~stateCount=...)`. `stateCount`
    is injected as a callback rather than read from `Registry` directly
    inside `Issue300Probe.res`, to avoid a real dependency cycle the compiler
    caught (`Editor` -> `Issue300Probe` -> `Registry` -> `State` -> `Goals` ->
    `Editor`).

- Persisted commands (registered only when `AGDA_ISSUE300_PROBE=1`):
  `agda-mode.issue300-probe-reset`, `-start`, `-stop`, `-dump`,
  `-visibleEditors`, `-stateCount`.

- How to run a focused issue-300 experiment with the persisted infrastructure,
  no editing of `TestSuiteAdapter.res` required:
  ```
  AGDA_ISSUE300_PROBE=1 AGDA_TEST_GLOB="Test__Issue300Smoke*.js" npm test
  ```
  Replace the glob (or add `AGDA_TEST_GREP="..."`) to target a future
  experiment's own test file instead. Drop `AGDA_ISSUE300_PROBE=1` to run any
  test without probe telemetry (e.g. a plain repeatability check that only
  needs `Issue300Harness.Warnings`).

- Smoke-test result: ran twice, both as intended.
  - Without `AGDA_ISSUE300_PROBE` set: `Issue300Harness.Probe.isEnabled()`
    correctly detects the probe commands are unregistered and the test
    no-ops (1 passing, no assertions attempted) -- confirming normal,
    unflagged `npm test` runs are unaffected.
  - With `AGDA_ISSUE300_PROBE=1` set: after activating the extension (opening
    one file first -- the probe's commands, like any other extension
    command, only exist post-activation), the test opened two no-real-data
    states, staled one via an H8-style tab switch, applied the combined
    stimulus, and asserted: the probe dump is non-empty, at least one dumped
    event is tagged something other than `live` (proving the probe actually
    observed the intended stale/other-live editor touch, not just unrelated
    `live` telemetry), `visibleEditors` is non-empty, `stateCount` is
    positive, and `Warnings.assertNone()` passes by actually reading the
    run's real `exthost.log` (independently confirmed to exist at the
    expected `<userDataDir>/logs/<timestamp>/window1/exthost/exthost.log`
    path). 1 passing, ~250ms. Two bugs were caught and fixed before this
    result: (1) a first version of this test failed with "command ... not
    found" because it checked `isEnabled()` before activating the extension
    at all -- fixed by activating first; (2) the original non-empty-dump-only
    assertion could have passed even if the H8 staling silently failed to
    produce any non-`live` tag -- fixed by asserting a non-`live` tag is
    present.
  - A normal, unrelated existing test (`Test__Main.res`) was also re-run with
    no env flags at all, to confirm the persisted changes don't affect
    ordinary test runs: passed as before.

- Explicit statement: **no product routing/fix behavior was introduced.**
  Every hook added to `Editor.res`/`Main.res` is a single no-op-when-disabled
  function call inserted before the pre-existing real operation; none of them
  branch on, delay, skip, or redirect any real listener/decoration/state
  logic. `Issue300Probe.enabled` is computed once from an environment
  variable that is unset in normal (non-test, non-flagged) use, so
  `registerCommands` never runs and `log`/`recordInitialize`/`recordCurrent`
  are no-ops in every real user session. One correction from initial review:
  `logSelectionEnter`/`logTextChangeEnter`/`logDecorationApply` originally
  computed their tag (`tagByEditor`/`tagByFileName`) *before* checking
  `enabled && capturing.contents`, and `tagByEditor` reads
  `editor->TextEditor.document` -- so every normal, unflagged run was still
  performing one extra `TextEditor.document` access per `setDecorations`
  call, immediately before it, which is exactly the kind of extra operation
  that could perturb the disposed-editor warning this investigation is about.
  Fixed by moving the `enabled && capturing.contents` check to wrap tag
  computation itself, not just the event recording, in all three functions.

- A second round of corrections from review, after the first round above was
  applied:
  - The new files were untracked (`git status` showed `??`) despite being
    documented as persisted infrastructure. Fixed by `git add`-staging
    `src/Issue300Probe.res`, `test/Issue300Harness.res`,
    `test/tests/Test__Issue300Smoke.res`, `test/RunTestFromCLI.res`,
    `test/TestSuiteAdapter.res`, `src/Editor.res`, `src/Main/Main.res`, and
    their compiled `lib/js/*.bs.js` outputs (staged only, not committed, per
    standing instructions to commit only when explicitly asked).
  - `Issue300Harness.Warnings.assertNone`/`assertCount` silently reported
    `exactCount: 0` whether the exthost.log genuinely contained zero
    warnings or simply couldn't be located (e.g. `AGDA_ISSUE300_USER_DATA_DIR`
    unset or the logs directory missing) -- making missing evidence
    indistinguishable from a real negative result. Fixed by having both
    functions check `result.logPath` and call `Mocha.Assert.fail(...)` with
    a descriptive message when it's `None`, instead of comparing a
    fabricated zero.
  - `Issue300Probe`'s `event` type only recorded `{label, tag, index}`,
    which is not enough to attribute a captured event to a specific file or
    call site -- the handoff had explicitly asked for per-file/state/context
    telemetry with a stable dump format. Fixed by adding `fileName: string`
    and `context: string` fields, threading `fileName` through `log`'s
    required arguments and `context` through an optional `~context=""`
    argument, and widening `dump`'s return type (and the harness's mirrored
    `Probe.dump` type) from a 3-tuple to a 5-tuple
    `(label, fileName, tag, context, index)`.
  - The event log (`events: ref<array<event>>`) grew without bound, despite
    the handoff explicitly asking for a bounded in-memory log. Fixed by
    adding `let maxEvents = 5000` and truncating the front of the array with
    `Array.sliceToEnd` inside `log` once the cap is exceeded.
  - `git diff --cached --check` failed on trailing whitespace in the three
    newly staged compiled `.bs.js` files (blank-line indentation the
    ReScript compiler emits, never previously enforced since it's already
    present throughout the existing tracked `lib/js/*.bs.js` corpus, but
    flagged here because every line of a brand-new file counts as
    "added"). The first cleanup attempt used
    `sed -i '' 's/[ \t]*$//'`, which on macOS's BSD sed does not treat `\t`
    inside a bracket expression as a tab escape -- it's read as two literal
    characters, backslash and the letter `t` -- so the bracket class
    `[ \t]` matched space, backslash, or `t` and silently truncated any line
    ending in a `t`, corrupting `assertCount: assertCount` into
    `assertCount: assertCoun` in the compiled harness output. The result was
    still syntactically valid JS (so `node -c` didn't catch it) but
    semantically broken, causing the extension host to crash with an opaque
    `[object Object]` and no stack trace when the smoke test ran. Diagnosed
    by confirming an unrelated pre-existing test still passed (ruling out
    general flakiness), then doing a clean `rm -rf lib/bs && npx rescript
    build` and diffing the fresh output against the sed-corrupted files,
    which showed the exact truncation. Fixed by re-running the strip with
    `sed -i '' 's/ *$//'` (spaces only) against the freshly rebuilt files,
    then verifying via `grep -n "assertCount"` (correct spelling restored),
    `node -c` (syntax valid), a trailing-whitespace grep (none found),
    a full webpack rebuild, and both smoke-test scenarios (disabled and
    enabled) passing again before re-staging.
  - After all of the above, `git diff --cached --check` exits 0, and
    `AGDA_TEST_GLOB="Test__Main.bs.js" npm test` (an unrelated pre-existing
    test) was re-run once more to confirm no regression.

- What future experiments no longer need to rebuild: the env-based test
  filters; ad hoc `/tmp` globbing and `grep -c` for the exact warning line;
  the per-file captured-vs-current editor tagging that took three rounds
  (H22's single global ref, H23's still-flawed version, H24's fix) to get
  right; opening no-real-data states, real loads, H8 staling, both switch
  primitives, and all four stimulus levels used across H20-H24. A future H26
  attempting rediscovery or attribution should be able to write only the
  experiment-specific variant logic and call into `Issue300Harness`/
  `Issue300Probe` for everything else.

### H26: consolidate the proven H22 natural reproduction

Hypothesis: H25's persisted infrastructure should be sufficient to turn H22's
strongest result into a compact, repeatable, automatic reproduction harness:
load a real Agda file (`Goals.agda`) so `Tokens` has non-empty decorations,
make that file's captured editor stale with the H8 tab-switch shape, trigger a
real text-change/IM stimulus, and observe the exact
`TextEditor is closed/disposed` warning from the real listener path. This is
not a new fix attempt and not the H22 phase-E/non-decoration search. It is a
consolidation step: prove that the known natural reproduction still works on
the current branch using only the reusable H25 lab.

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch. Do not add
   any behavior-changing gate, listener filter, editor reroute, or product fix.
   H26 is reproduction-only.
2. Add one focused experiment test, preferably
   `test/tests/Test__Issue300H26.res`, using the persisted H25 helpers:
   - enable probe use by requiring the run to set `AGDA_ISSUE300_PROBE=1`;
   - activate/load the real Agda fixture with
     `Issue300Harness.Scenario.loadReal("Goals.agda")`;
   - reset/start the probe only after extension activation;
   - create the H8 stale-captured-editor shape for `Goals.agda` by switching to
     another Agda file such as `InputMethod.agda` and then back to
     `Goals.agda` via `Issue300Harness.Scenario.staleViaTabSwitch`;
   - trigger the natural listener path with a small deterministic text-change
     stimulus. Start with the H22 shape: 10 IM cycles or the smallest subset
     that still reproduces. Do not use a direct replay command or an isolated
     `setDecorations` command as the primary evidence.
3. The test must capture, at minimum:
   - `Issue300Harness.Warnings.capture()` result: log path, exact count, first
     and last exact warning line;
   - probe dump filtered to `label == "Decoration.apply"` for `Goals.agda`;
   - number of stale `Decoration.apply` events for `Goals.agda`;
   - total selection/text-change events observed for `Goals.agda`;
   - `visibleEditors` and `stateCount` before cleanup.
4. Success criteria for a single run:
   - exact warning count is greater than zero;
   - the probe dump contains at least one
     `("Decoration.apply", <Goals.agda path>, "stale", ..., ...)` event;
   - the warning appears after the stale setup and after the real text-change
     stimulus, not during setup alone;
   - no test-only replay/forced warning command is used.
5. Repeatability criteria:
   - run the focused test in at least three independent `npm test` invocations,
     not three `it` blocks in one extension-host process. Use:
     ```
     AGDA_ISSUE300_PROBE=1 AGDA_TEST_GLOB="Test__Issue300H26*.js" npm test
     ```
   - each invocation must produce `exactCount > 0` and stale
     `Decoration.apply` evidence. Record the per-run counts separately.
   - if any run has `exactCount == 0`, classify H26 as not deterministic and
     record the telemetry difference between successful and failed runs.
6. If the first H22-shaped stimulus does not reproduce, do not immediately
   expand into a broad matrix. Try only these narrow escalations, one at a time,
   recording each as its own row:
   - increase IM cycles from the minimum to 10, then 50;
   - add a 0ms microtask delay after tab staling, then 50ms;
   - use `typeRevertStimulus` instead of IM cycles;
   - use `switchViaNextEditor` only if the direct H8 `showTextDocument` switch
     path does not reproduce.
7. Result classification:
   - `confirmed deterministic reproduction`: all three independent focused runs
     meet the repeatability criteria above. Preserve the minimal H26 test as
     the automatic reproduction artifact.
   - `partially reproduced`: at least one run emits the exact warning with stale
     `Decoration.apply` evidence, but not all runs do. Preserve the best
     reproducer only if it is compact and clearly documented as intermittent.
   - `not reproduced`: no focused H26 variant emits the exact warning. Remove
     the temporary H26 test and record that H22's natural reproduction did not
     survive consolidation on the current branch/harness.
   - `invalid`: warning capture cannot find `exthost.log`, probe commands are
     not registered despite `AGDA_ISSUE300_PROBE=1`, or the stale
     `Decoration.apply` condition is never achieved.
8. Update this document with:
   - exact test file(s) added and whether they were preserved or removed;
   - exact command(s) run;
   - per-run warning counts and stale `Decoration.apply` counts;
   - first/last raw exact warning line for each reproducing run;
   - whether this establishes an automatic deterministic reproduction of #300
     on the real listener path.
9. Cleanup:
   - keep only the minimal reproduction test if H26 is confirmed deterministic
     or intentionally preserved as an intermittent reproducer;
   - remove exploratory variants, scratch files, temporary logs, and any
     failed helper drafts;
   - leave the persisted H25 infrastructure intact;
   - run `npx rescript build`, `npx webpack --mode development`, and
     `git diff --check` / `git diff --cached --check` before handing back.

### H26 Results (2026-07-09)

**Classification: confirmed deterministic reproduction.** Added
`test/tests/Test__Issue300H26.res`, built entirely on the persisted H25
`Issue300Harness`/`Issue300Probe` lab (no new instrumentation): it activates
cheaply via `Scenario.openNoLoad("InputMethod.agda")` first so `Probe.isEnabled`
reflects `AGDA_ISSUE300_PROBE` rather than "not activated yet", then --
only when the probe is enabled -- resets/starts the probe (before the real
load, immediately after that cheap activation), does a real load of
`Goals.agda` (`Scenario.loadReal`, giving `Tokens` non-empty decorations, the
H22 phase C precondition), stales that captured editor via
`Scenario.staleViaTabSwitch("Goals.agda", "InputMethod.agda")` (the H8 shape),
captures a `Warnings.capture()` baseline right after staling but *before* the
stimulus, and then runs the H22 phase C stimulus of 10
`Scenario.imCycleStimulus()` calls before capturing `Warnings.capture()`
again. It then also captures the probe dump filtered to
`label == "Decoration.apply"` and `fileName` containing `Goals.agda`
(further filtered to `tag == "stale"`), the count of `Goals.agda`
selection/text-change listener events, `visibleEditors`, and `stateCount`.
Without `AGDA_ISSUE300_PROBE=1` it detects the probe commands aren't
registered and no-ops, exactly like the H25 smoke test.

Command used for all runs: `AGDA_ISSUE300_PROBE=1
AGDA_TEST_GLOB="Test__Issue300H26*.js" npm test`. Ran three independent
process invocations (separate `npm test` calls, not separate `it` blocks):

| Run | exactCount before stimulus | exactCount after stimulus | stale `Decoration.apply` (`Goals.agda`) | `Goals.agda` listener events | visibleEditors | stateCount | first exact warning line | last exact warning line |
|-----|----------------------------:|---------------------------:|------------------------------------------:|-------------------------------:|-----------------:|-------------:|---------------------------|--------------------------|
| 1 | 0 | 40 | 40 | 42 | 1 | 2 | `2026-07-09 12:54:41.636 [warning] TextEditor is closed/disposed` | `2026-07-09 12:54:41.696 [warning] TextEditor is closed/disposed` |
| 2 | 0 | 40 | 40 | 42 | 1 | 2 | `2026-07-09 12:54:48.839 [warning] TextEditor is closed/disposed` | `2026-07-09 12:54:48.904 [warning] TextEditor is closed/disposed` |
| 3 | 0 | 40 | 40 | 42 | 1 | 2 | `2026-07-09 12:54:55.753 [warning] TextEditor is closed/disposed` | `2026-07-09 12:54:55.827 [warning] TextEditor is closed/disposed` |

All three runs satisfy every success/repeatability criterion in the handoff:
`exactCount` after the stimulus is greater than zero and strictly greater
than the count captured right after staling but before the stimulus (0 in
every run) -- proving the warning is produced by the stimulus itself, not by
the real-load/tab-switch setup alone; the dump contains multiple
`("Decoration.apply", <Goals.agda path>, "stale", ..., ...)` events (every
captured `Decoration.apply` call for `Goals.agda` in each run was tagged
`stale`, i.e. `staleGoalsDecorationEvents == goalsDecorationEvents` in all
three runs); and no test-only replay/forced-warning command was used anywhere
in the test. A separate unflagged run (`AGDA_TEST_GLOB="Test__Issue300H26*.js"
npm test`, no `AGDA_ISSUE300_PROBE`) confirmed the dormancy no-op path still
works: the test logs the "probe not registered" message and passes trivially.

This establishes an automatic, deterministic reproduction of the exact #300
warning on the real listener path, repeatable across independent processes
with a single compact test, superseding the need to reconstruct H22 phase C's
setup by hand. Note the per-run count (40 in all three runs here) is higher
than H22 phase C's original 5/5 -- consistent with H22's own phase G/H finding
that repeated/extended stimulus does not scale the stale-call count in a
simple linear way; the mechanism remains bounded and self-limiting per run,
just not at the same constant H22 originally measured. Flooding (the reported
"hundreds per keypress") is still not what's observed here: these are tens of
occurrences from ten IM cycles, not hundreds from one keypress. The phase-E
non-decoration warning source (H23/H24, still unattributed) is out of scope
for H26 and remains open.

#### Cleanup

`test/tests/Test__Issue300H26.res` is preserved as the permanent
reproduction artifact (per the handoff's "confirmed deterministic
reproduction" branch) -- no exploratory variants, scratch files, or temporary
logs were left behind (the `imCycleStimulus` count of 10 and the phase C
scenario shape were used directly, without needing escalation). The persisted
H25 infrastructure (`src/Issue300Probe.res`, `test/Issue300Harness.res`,
`test/tests/Test__Issue300Smoke.res`) was not modified. Ran `npx rescript
build`, `npx webpack --mode development`, and confirmed `git diff --check`
(nothing to check outside staged content) and `git diff --cached --check`
both exit 0 after staging the new test file and its compiled output.

### H27: reduce the H26 reproducer and measure stimulus scaling

Hypothesis: H26's deterministic reproduction is real, but it is not yet
minimal. The exact warning may be triggered by a much smaller stimulus than ten
IM cycles after the real-load + H8 stale setup, and the warning count may either
be roughly proportional to stale `Decoration.apply` calls or capped by a
self-limiting decoration state. H27 should keep H26 as the positive control and
answer two concrete questions:

1. What is the smallest automatic stimulus that still reproduces the exact
   warning after the H26 setup?
2. Does warning count scale toward the reported "hundreds per keypress"
   flooding, or remain bounded/tens-per-run under the tested variants?

Lab assistant task:

1. Work on `issue#300-new` without applying any routing/fix patch. Do not add a
   document guard, stale-editor guard, listener reroute, debounce, skip, or any
   product behavior change. H27 is reproduction characterization only.
2. Start from the preserved H26 setup:
   - activate cheaply with `Issue300Harness.Scenario.openNoLoad("InputMethod.agda")`;
   - require `AGDA_ISSUE300_PROBE=1` and no-op when the probe is absent;
   - `Probe.reset()` / `Probe.start()`;
   - `Issue300Harness.Scenario.loadReal("Goals.agda")`;
   - `Issue300Harness.Scenario.staleViaTabSwitch("Goals.agda", "InputMethod.agda")`;
   - capture `Warnings.capture()` immediately after staling and before each
     stimulus variant.
3. Add one focused temporary experiment test, preferably
   `test/tests/Test__Issue300H27.res`. Keep H26 unchanged as the permanent
   positive-control artifact.
4. Run each variant in a fresh `npm test` process. Do not pack multiple
   variants into separate `it` blocks in one extension-host process, because
   prior rounds repeatedly found cross-test Registry/editor contamination.
   Use env filtering for one variant at a time, for example:
   ```
   AGDA_ISSUE300_PROBE=1 AGDA_TEST_GLOB="Test__Issue300H27*.js" AGDA_TEST_GREP="<variant name>" npm test
   ```
5. Minimum required variant matrix:
   - `control-10-im-cycles`: exactly the H26 stimulus, 10
     `imCycleStimulus()` calls. Expected to reproduce; if it does not, stop and
     classify H27 as invalid because the control no longer matches H26.
   - `one-im-cycle`: one `imCycleStimulus()` call.
   - `one-type-revert`: one `typeRevertStimulus()` call, no IM activation cycle.
   - `cursor-only`: one `cursorStimulus()` call, no text edit.
   - `no-stimulus-delay`: no edit/selection stimulus after staling; wait 250ms,
     then capture warnings. This is the negative control for delayed setup-only
     warnings.
   - `fifty-im-cycles`: 50 `imCycleStimulus()` calls, only if `one-im-cycle`
     reproduces or the control reproduces with exactly the expected stale
     `Decoration.apply` evidence. This tests scaling without a broad matrix.
6. For every variant, record:
   - exact command run;
   - warning count before stimulus;
   - warning count after stimulus;
   - delta warning count;
   - total `Decoration.apply` events for `Goals.agda`;
   - stale `Decoration.apply` events for `Goals.agda`;
   - `Goals.agda` selection/text-change listener event count;
   - `visibleEditors` and `stateCount`;
   - first/last exact warning line, if any;
   - whether every `Goals.agda` decoration event was tagged `stale`.
7. Run repeatability only for the smallest reproducing variant:
   - once the smallest variant with `delta warning count > 0` and stale
     `Decoration.apply` evidence is found, run that same variant three
     independent times;
   - if all three reproduce, classify it as the new minimal deterministic
     reproducer;
   - if only some reproduce, classify it as intermittent and keep H26 as the
     deterministic reproducer.
8. Result classification:
   - `minimal deterministic reproducer found`: a smaller-than-H26 stimulus
     reproduces in all three independent repeat runs with stale
     `Decoration.apply` evidence. Preserve a compact permanent test only if it
     should replace or supplement H26.
   - `H26 remains minimal deterministic reproducer`: smaller variants do not
     reproduce deterministically, while the 10-cycle control does.
   - `scaling/flooding observed`: warning count grows sharply enough to approach
     the reported flooding shape, e.g. substantially more than H26's tens per
     run or hundreds from a single key/edit stimulus. Preserve the scaling test
     only if it is compact and deterministic.
   - `bounded/self-limiting`: the warning count remains in the same bounded
     range as H26, even with 50 IM cycles.
   - `invalid`: H26 control no longer reproduces, warning logs cannot be found,
     probe commands are absent despite `AGDA_ISSUE300_PROBE=1`, or stale
     `Decoration.apply` evidence is missing from a claimed reproducing run.
9. Update this document with:
   - the variant table and repeatability table;
   - which variant is now the smallest reproducer;
   - whether H26's 10-cycle permanent test remains the best deterministic
     artifact;
   - whether flooding is reproduced, bounded, or still open.
10. Cleanup:
    - if H27 finds a better permanent minimal reproducer, preserve only the
      compact test(s) needed for that artifact and their compiled output;
    - otherwise remove `Test__Issue300H27.res` and its compiled artifacts after
      recording the results, leaving H26 as the permanent reproducer;
    - leave H25 infrastructure and H26 unchanged unless explicitly documenting
      a new permanent artifact;
    - run `npx rescript build`, `npx webpack --mode development`,
      `git diff --check`, and `git diff --cached --check` before handing back.

### H27 Results (2026-07-09)

**Classification: minimal deterministic reproducer found, and scaling
observed (not bounded/self-limiting).** Reused H26's exact setup (cheap
activation, `AGDA_ISSUE300_PROBE=1` gate, `Scenario.loadReal("Goals.agda")`,
`Scenario.staleViaTabSwitch("Goals.agda", "InputMethod.agda")`), capturing
`Warnings.capture()` immediately after staling and again after each
stimulus. The initial exploratory pass put all six variants as `it` blocks
in one file selected by `AGDA_TEST_GREP`; a first attempt using
`AGDA_TEST_GREP="minimal reproducer"` ran two variants together in one
extension-host process, because mocha's grep matches the full
`describe`+`it` title and the `describe` block's own name contained that
phrase. Rather than rely on remembering the right grep string, the two
variants worth preserving were each split into their own file
(`test/tests/Test__Issue300H27MinimalReproducer.res`,
`test/tests/Test__Issue300H27Scaling.res`) so that selecting either by
`AGDA_TEST_GLOB` alone -- no `AGDA_TEST_GREP` needed -- runs exactly one
variant in its own fresh process, structurally enforcing the handoff's
per-variant fresh-process rule instead of depending on a correctly-typed
grep pattern each time.

Exploratory variant matrix (one fresh process per row, command
`AGDA_ISSUE300_PROBE=1 AGDA_TEST_GLOB="Test__Issue300H27*.js" AGDA_TEST_GREP="<variant>"`,
`Goals.agda` loaded and staled the same way in every row):

| Variant | Stimulus | exactCount before | exactCount after | delta | total `Decoration.apply` (`Goals.agda`) | stale `Decoration.apply` | all decorations stale? | listener events (`Goals.agda`) | visibleEditors | stateCount | first exact warning line | last exact warning line |
|---------|----------|-------------------:|-------------------:|------:|-------------------------------------------:|----------------------------:|:-------------------------|----------------------------------:|------------------:|-------------:|---------------------------|--------------------------|
| control-10-im-cycles | 10x `imCycleStimulus` (H26 shape) | 0 | 40 | 40 | 87 | 40 | false | 42 | 1 | 2 | `2026-07-09 13:08:24.852 [warning] TextEditor is closed/disposed` | `2026-07-09 13:08:24.916 [warning] TextEditor is closed/disposed` |
| one-im-cycle | 1x `imCycleStimulus` | 0 | 6 | 6 | 35 | 6 | false | 7 | 1 | 2 | `2026-07-09 13:08:33.535 [warning] TextEditor is closed/disposed` | `2026-07-09 13:08:33.540 [warning] TextEditor is closed/disposed` |
| one-type-revert | 1x `typeRevertStimulus` | 0 | 4 | 4 | 31 | 4 | false | 6 | 1 | 2 | `2026-07-09 13:08:41.041 [warning] TextEditor is closed/disposed` | `2026-07-09 13:08:41.046 [warning] TextEditor is closed/disposed` |
| cursor-only | 1x `cursorStimulus`, no edit | 0 | 0 | 0 | 27 | 0 | false | 3 | 1 | 2 | none | none |
| no-stimulus-delay | 250ms wait, no stimulus | 0 | 0 | 0 | 27 | 0 | false | 2 | 1 | 2 | none | none |
| fifty-im-cycles | 50x `imCycleStimulus` | 0 | 206 | 206 | 334 | 206 | false | 206 | 1 | 2 | `2026-07-09 13:09:25.215 [warning] TextEditor is closed/disposed` | `2026-07-09 13:09:25.412 [warning] TextEditor is closed/disposed` |

The control reproduced with the same shape as H26 (confirming H27's setup
matches H26's). `cursor-only` and `no-stimulus-delay` both produced zero
warnings, confirming staling alone (with or without a delay) does not
produce the warning -- an actual text-change/IM stimulus is required.
`one-type-revert` was the smallest stimulus tested that reproduced, smaller
than a full IM activate/type/type/escape cycle. In every row, `delta` equals
the stale `Decoration.apply` count exactly, even though `allDecorationsStale`
is false in every row (other, non-stale decoration calls also occurred, e.g.
from `IM.res`'s live-editor underline decoration) -- the exact 1:1
correspondence is specifically between the warning delta and the *stale*
subset of decoration calls, not between the warning delta and all decoration
activity.

Repeatability, run on the split `Test__Issue300H27MinimalReproducer.res`
(three independent fresh processes, command `AGDA_ISSUE300_PROBE=1
AGDA_TEST_GLOB="Test__Issue300H27MinimalReproducer*.js" npm test`, no grep
needed):

| Run | delta | total `Decoration.apply` | stale `Decoration.apply` | listener events | visibleEditors | stateCount |
|-----|------:|----------------------------:|----------------------------:|-------------------:|------------------:|-------------:|
| 1 | 4 | 31 | 4 | 6 | 1 | 2 |
| 2 | 4 | 31 | 4 | 6 | 1 | 2 |
| 3 | 4 | 31 | 4 | 6 | 1 | 2 |

All three runs reproduced identically. **Classification: minimal
deterministic reproducer found.** A single type-then-revert edit after the
H26 setup is now the smallest known automatic reproducer for #300's exact
warning, and it is preserved as a permanent test with an exact-equality
assertion (`delta == staleDecorationCount`), not just a "some evidence"
check.

Scaling, run via `AGDA_ISSUE300_PROBE=1
AGDA_TEST_GLOB="Test__Issue300H27Scaling*.js" npm test` (no grep needed):
delta=206, total `Decoration.apply` for `Goals.agda`=334, stale
`Decoration.apply`=206 (again exactly equal to delta), listener events=206,
visibleEditors=1, stateCount=2, against H26's 10-cycle baseline of ~40 --
roughly 5x the warnings for 5x the stimulus. This directly contradicts H22
phases G/H, which had found the stale-call count stayed fixed at 5 regardless
of 10 vs. 100 cycles or repeated tab-switching, for that round's setup.
**Classification: scaling observed**, not bounded/self-limiting, though still
short of the reported "hundreds of warnings per keypress" (this is hundreds
of warnings across 50 edit cycles, not from a single keystroke). This
variant is preserved as a permanent test asserting both `delta > 40` (to
catch a regression back to bounded behavior) and the same exact
`delta == staleDecorationCount` equality.

#### Cleanup

The original single-file, six-variant exploratory test
(`test/tests/Test__Issue300H27.res`) was deleted after recording the matrix
above. The two variants worth preserving as permanent artifacts --
`one-type-revert` and `fifty-im-cycles` -- were rewritten as two standalone
files, `test/tests/Test__Issue300H27MinimalReproducer.res` and
`test/tests/Test__Issue300H27Scaling.res`, each capturing and logging the
full field set (before/after/delta exact counts, total and stale
`Decoration.apply` counts for `Goals.agda`, whether every decoration event
was stale, `Goals.agda` listener event count, `visibleEditors`, `stateCount`,
first/last exact warning line) and asserting `delta > 0`
(`Test__Issue300H27MinimalReproducer.res`) or `delta > 40`
(`Test__Issue300H27Scaling.res`), plus the exact-equality
`delta == staleDecorationCount` assertion in both. `control-10-im-cycles`,
`one-im-cycle`, `cursor-only`, and `no-stimulus-delay` were
exploratory/negative-control only and were not preserved beyond the matrix
above; H26 remains unchanged as the primary positive-control artifact. Ran
`npx rescript build`, `npx webpack --mode development`, and confirmed
`git diff --check` / `git diff --cached --check` both exit 0 after staging.
Also confirmed the real-Agda-load stimulus does not permanently mutate
`test/tests/assets/Goals.agda`'s tracked content: an earlier run left a
modified working copy (case-split markers changed from `?` to `{!   !}`) as
a side effect of repeatedly loading/typing into the fixture, which was
reverted with `git checkout -- test/tests/assets/Goals.agda` before staging.

### H28: consolidate the permanent reproduction surface

Hypothesis: H26-H27 achieved deterministic automatic reproduction, but the
retained test surface is still shaped like the investigation log rather than a
maintainable project test. H28 is not an open-ended audit. The design decision
is already made here: reduce the permanent surface to:

1. normal `npm test` execution with a fresh VS Code profile;
2. one focused file selector for fresh-process reproduction runs;
3. a generic extension-host log helper;
4. one behavior-named deterministic reproduction test.

Everything else from H25-H27 is discovery scaffolding and should be removed
from the permanent suite. The executor's job is to implement this plan, verify
that the deterministic reproduction still works, record the result, and stop if
the prescribed plan contradicts observed behavior.

This is consolidation only. Do not fix #300 in this round. Do not add a
document guard, stale-editor guard, listener reroute, debounce, skip, or any
behavior-changing product code.

Lab assistant task:

1. Work on `issue#300-new`. Start by recording the current baseline command
   and results, without changing code:
   ```
   AGDA_ISSUE300_PROBE=1 AGDA_TEST_GLOB="Test__Issue300H27MinimalReproducer*.js" npm test
   ```
   If this baseline no longer reproduces as documented in H27, stop and record
   the failure before doing cleanup.
2. Apply this fixed artifact plan. Do not ask the executor to choose among
   these classifications:
   - keep `test/RunTestFromCLI.res`'s fresh `--user-data-dir` /
     `--extensions-dir` setup;
   - keep `test/TestSuiteAdapter.res` file-level selection via
     `AGDA_TEST_GLOB`;
   - remove `AGDA_TEST_GREP`;
   - remove `AGDA_TEST_TIMEOUT`;
   - rename `AGDA_ISSUE300_USER_DATA_DIR` to generic
     `AGDA_TEST_USER_DATA_DIR`;
   - move exact extension-host log lookup/counting into
     `test/tests/Test__Util.res`;
   - remove `test/Issue300Harness.res`;
   - remove `src/Issue300Probe.res`, its call sites, its commands, and
     `AGDA_ISSUE300_PROBE`;
   - replace `test/tests/Test__Issue300H27MinimalReproducer.res` with
     `test/tests/Test__StaleEditorDecorationWarning.res`;
   - remove `test/tests/Test__Issue300Smoke.res`;
   - remove `test/tests/Test__Issue300H26.res`;
   - remove `test/tests/Test__Issue300H27Scaling.res`;
   - remove compiled `lib/js` outputs for deleted ReScript files and rebuild
     compiled output for retained/renamed files.
3. Preserve normal test-runner behavior. `npm test` must remain the only test
   entry point. The fresh `--user-data-dir` / `--extensions-dir` setup in
   `RunTestFromCLI.res` should stay because it is general VS Code test
   isolation, not issue-specific infrastructure.
4. In `test/TestSuiteAdapter.res`, keep only:
   - default glob `**/*.js`;
   - `AGDA_TEST_GLOB` override.
   Delete the Mocha `grep` binding/use and the timeout env override. Leave the
   default timeout as ordinary code, not an environment-controlled API.
5. In `test/RunTestFromCLI.res`, keep the fresh temp root and launch args.
   Export the temp user-data directory as `AGDA_TEST_USER_DATA_DIR`. Rewrite
   comments so they explain VS Code test isolation and log discovery only.
6. In `test/tests/Test__Util.res`, add a small generic helper for extension-host
   log capture. It should:
   - read `AGDA_TEST_USER_DATA_DIR`;
   - locate the current `logs/<latest>/window1/exthost/exthost.log`;
   - fail loudly when the log cannot be found;
   - count exact lines containing a caller-provided phrase;
   - expose no issue-numbered names.
7. Create `test/tests/Test__StaleEditorDecorationWarning.res` as the only
   retained permanent reproduction test. It should:
   - use `open Mocha` and `open Test__Util`;
   - describe the test as `stale editor decoration warning (#300)`;
   - locally define the small sequence helpers it needs instead of importing a
     harness: open no-load file via input-method activate/escape, load
     `Goals.agda`, stale it by switching to `InputMethod.agda` and back, type
     one character, delete it;
   - capture exact `TextEditor is closed/disposed` warning count before and
     after the type/delete stimulus using the new generic log helper;
   - assert `after - before > 0`;
   - not use `Issue300Harness`, `Issue300Probe`, `AGDA_ISSUE300_PROBE`, or
     any H-numbered name.
8. Delete the redundant permanent artifacts:
   - `test/Issue300Harness.res`;
   - `src/Issue300Probe.res`;
   - `test/tests/Test__Issue300Smoke.res`;
   - `test/tests/Test__Issue300H26.res`;
   - `test/tests/Test__Issue300H27MinimalReproducer.res`;
   - `test/tests/Test__Issue300H27Scaling.res`;
   - all corresponding compiled `lib/js` outputs.
9. Remove all `Issue300Probe` imports/call sites from product code. This is
   deletion of test-only instrumentation, not a #300 behavior fix. Do not add
   any replacement guard or routing behavior.
10. Rebuild generated outputs for retained ReScript sources.
11. Verify after cleanup:
    - run the retained deterministic reproduction command:
      ```
      AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test
      ```
    - run normal `npm test`;
    - run `npx rescript build`;
    - run `npx webpack --mode development`;
    - run `git diff --check` and `git diff --cached --check`.
12. If the new no-probe reproduction test does not reproduce the exact warning,
    stop. Do not reintroduce the old probe or harness on your own. Record the
    failed command, before/after counts, and the exact observed log path/state
    so the principal investigator can redesign the plan.
13. Update this document with:
    - the fixed classification table from step 2 and what changed;
    - the retained reproduction command;
    - the deleted/renamed files;
    - explicit note that scaling remains historical evidence in H27, not a
      permanent test;
    - explicit note that stale attribution remains historical evidence from
      H26-H27, and the permanent test now asserts the user-visible warning only;
    - all verification commands and results.
14. Cleanup the lab:
    - remove scratch logs and one-off files;
    - leave `test/tests/assets/Goals.agda` and all other fixtures unmodified;
    - leave the working tree containing only intentional source/test/docs and
      generated-output changes.

### H28 Results (2026-07-09)

**Classification: consolidated as planned; deterministic reproduction
confirmed without the probe.** Baseline command
`AGDA_ISSUE300_PROBE=1 AGDA_TEST_GLOB="Test__Issue300H27MinimalReproducer*.js" npm test`
reproduced as documented in H27 (delta=4) before any code changes, so the
fixed plan from the handoff was applied as-is with no deviation and no need
to invoke the "stop and redesign" branch.

Final classification table (all items from the handoff's step 2, applied
exactly):

| Artifact | Disposition |
|----------|-------------|
| `test/RunTestFromCLI.res` | kept; fresh `--user-data-dir`/`--extensions-dir` setup unchanged (general VS Code test isolation); exported env var renamed `AGDA_ISSUE300_USER_DATA_DIR` -> `AGDA_TEST_USER_DATA_DIR`; comments now describe test isolation/log discovery only, no issue-300 wording |
| `test/TestSuiteAdapter.res` | kept, trimmed; `AGDA_TEST_GLOB` retained (default `**/*.js`); `AGDA_TEST_GREP` and its `Mocha.grep` binding removed; `AGDA_TEST_TIMEOUT` removed, default timeout (4000ms) is now plain code, not env-controlled |
| `test/tests/Test__Util.res` | extended with a new `ExtHostLog` module (`locate`, `countLinesContaining`) -- generic, no issue-numbered names, fails loudly via `Assert.fail` when the log can't be located |
| `test/Issue300Harness.res` | removed entirely; its `Warnings` capture/counting logic was generalized into `Test__Util.ExtHostLog`; its `Probe` wrappers were removed along with the probe; its `Scenario` helpers were not generalized -- reproduction-specific sequencing (open-no-load, stale-via-tab-switch, one type/delete edit) is now written locally, inline, in the one retained test |
| `src/Issue300Probe.res` | removed entirely, along with its call sites in `src/Editor.res` (`Decoration.apply`) and `src/Main/Main.res` (`initialize`, both global listeners, `onOpenEditor`, `activateWithoutContext`) and the `AGDA_ISSUE300_PROBE` env var. Exact exthost-log warning-count capture is sufficient for future fix validation; stale-`Decoration.apply` attribution was an investigative question already answered (H22/H26/H27) and is not needed for regression detection going forward |
| `test/tests/Test__Issue300Smoke.res` | removed; it only smoke-tested the probe/harness plumbing, which no longer exists |
| `test/tests/Test__Issue300H26.res` | removed; superseded by the smaller one-edit reproducer |
| `test/tests/Test__Issue300H27MinimalReproducer.res` | removed; replaced by `test/tests/Test__StaleEditorDecorationWarning.res` |
| `test/tests/Test__Issue300H27Scaling.res` | removed; scaling remains historical evidence in the H27 Results section only, not a permanent test |
| new: `test/tests/Test__StaleEditorDecorationWarning.res` | the sole retained permanent reproduction test. `describe("stale editor decoration warning (#300)", ...)`; locally defines the open-no-load, real-load, stale-via-tab-switch, and one-character type/delete sequence; captures `Test__Util.ExtHostLog.countLinesContaining("TextEditor is closed/disposed")` before and after the edit; asserts `after - before > 0`; imports nothing from the deleted harness/probe and uses no H-numbered or issue-numbered names beyond the `describe` string (for provenance) |

Retained reproduction command: `AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test`
(no `AGDA_ISSUE300_PROBE` needed anymore -- the probe is gone -- and no grep
needed, since the file selector alone isolates the one test). Ran it three
independent times; all three passed (523ms, 320ms, 308ms), and
`test/tests/assets/Goals.agda` was confirmed unmodified after each run.

Full-suite verification: `npm test` (no glob) was run twice. The first run
reported 858 total (857 passing, 1 failing: `Connection > make with logging
> should log connection events when using real agda command`, a
`This.timeout(10000)` exceeded on a real spawned `agda` subprocess, in
`test/tests/Test__Connection.res` -- a file untouched by this round's
changes). The second run, immediately after, passed all 858 with no code
changes in between (32s vs. the first run's 50s), confirming the failure was
transient real-subprocess timing flakiness pre-existing on this branch, not a
regression introduced by H28's consolidation.

Also ran (both succeeded): `npx rescript build`, `npx webpack --mode
development`, `npx webpack --env target=web --mode development` (the web
bundle target, since `Editor.res`/`Main.res` changes affect both bundles),
`git diff --check`, `git diff --cached --check`. One recurring whitespace
regression (the same ReScript-codegen blank-line-with-trailing-space pattern
noted in H25/H26/H27) reappeared in freshly rebuilt `lib/js/test/tests/Test__Util.bs.js`
and the other touched `lib/js/*.bs.js` outputs after a rebuild; cleaned with
`sed -i '' 's/ *$//' <file>` (space-only pattern, not the BSD-`\t`-bracket
bug from H25) and reverified with `node -c` and a rebuild before staging.

Explicit notes per the handoff's step 13:
- Scaling remains historical evidence in the H27 Results section only; it is
  not exercised by any permanent test going forward.
- Stale `Decoration.apply` attribution remains historical evidence from
  H26-H27 (the exact call site and mechanism are documented there); the
  permanent test now asserts only the user-visible exact warning count via
  the extension host log, with no attribution telemetry.
- The probe (`src/Issue300Probe.res`) and its production-code call sites are
  gone; `AGDA_ISSUE300_PROBE` no longer does anything (it isn't read anywhere
  in the codebase).

#### Cleanup

No scratch logs or one-off files were left in the repo (all diagnostic
`npm test` output for this round was written under the session's own
scratchpad directory, outside the repo). `test/tests/assets/Goals.agda` was
confirmed unmodified. The working tree contains only the intentional
source/test/doc changes listed in the classification table above, plus their
compiled `lib/js` counterparts; `git diff --check` and `git diff --cached
--check` both exit 0.

### H29: remove env-based log-path plumbing via ExtensionContext.logUri

Hypothesis: `AGDA_TEST_USER_DATA_DIR` is no longer necessary. The running
extension already receives a real `ExtensionContext`, and VS Code exposes
`ExtensionContext.logUri`, a directory for extension logs. In the desktop
extension-test host, that directory should live below the same
`logs/<timestamp>/window1/exthost/` directory as `exthost.log`, so the test
can derive the exact extension-host log path from the activated extension's
exported `logUri` instead of passing the user-data directory through an
environment variable.

This is infrastructure cleanup only. Do not change the #300 reproduction
sequence. Do not add a document guard, stale-editor guard, listener reroute,
debounce, skip, or any behavior-changing product code.

Lab assistant task:

1. Work on `issue#300-new` after H28. Start by running the retained
   reproduction exactly once, without changing code:
   ```
   AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test
   ```
   If this no longer reproduces, stop and record the failure before changing
   anything.
2. Remove `AGDA_TEST_USER_DATA_DIR` as a runner/test contract:
   - delete the `Dict.set("AGDA_TEST_USER_DATA_DIR", testUserDataDir)` call
     from `test/RunTestFromCLI.res`;
   - remove comments that describe exporting that env var;
   - keep the fresh `--user-data-dir` / `--extensions-dir` launch args;
   - update generated `lib/js/test/RunTestFromCLI.bs.js`.
3. Export the activated extension's log URI:
   - add `logUri: VSCode.Uri.t` to `Main.activationExports`;
   - in `Main.activate`, read `VSCode.ExtensionContext.logUri(context)`;
   - return `{channels, memento, logUri}`;
   - update generated `lib/js/src/Main/Main.bs.js`.
4. Cache the activated extension's `logUri` in `test/tests/Test__Util.res`.
   Preserve the existing `activateExtension` / `activateExtensionAndOpenFile`
   call shape for tests. Add a small internal ref for the activated
   extension's log URI when `activateExtension` receives `exports`.
5. Rewrite `Test__Util.ExtHostLog.locate` to use the cached activation
   `logUri`, not the environment:
   - fail loudly if the extension has not been activated yet;
   - convert `logUri` to a filesystem path;
   - derive candidate `exthost.log` as the sibling of the extension log
     directory, i.e. `dirname(logUri.fsPath)/exthost.log`;
   - if that exact candidate does not exist, fail loudly and include the
     observed `logUri.fsPath` and candidate path in the error message;
   - do not glob `/tmp`;
   - do not reintroduce an environment variable or sidecar file.
6. Temporarily log or assert the observed path shape during the first focused
   run. The expected shape is:
   ```
   <user-data>/logs/<timestamp>/window1/exthost/<extension-id-or-name>
   <user-data>/logs/<timestamp>/window1/exthost/exthost.log
   ```
   If the shape is different but there is still a deterministic derivation
   from `logUri` to `exthost.log`, implement that derivation and document the
   observed shape. If there is no deterministic derivation from `logUri`, stop
   and record the observed `logUri`, nearby directory listing, and failure
   reason; do not invent a new fallback.
7. Keep `Test__StaleEditorDecorationWarning.res` behavior unchanged except for
   any import/helper name adjustment required by the new `ExtHostLog`
   implementation. It should still assert only that the exact
   `TextEditor is closed/disposed` warning count increases after the one
   type/delete stimulus.
8. Search the codebase and generated outputs for stale plumbing:
   - `AGDA_TEST_USER_DATA_DIR`;
   - `AGDA_ISSUE300_USER_DATA_DIR`;
   - `AGDA_ISSUE300_PROBE`;
   - `Issue300Harness`;
   - `Issue300Probe`.
   The first three should not remain in source/test/generated code after this
   cleanup, except in historical documentation sections.
9. Rebuild generated outputs for retained ReScript sources.
10. Verify:
    - run the focused reproduction:
      ```
      AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test
      ```
    - run normal `npm test`;
    - run `npx rescript build`;
    - run `npx webpack --mode development`;
    - run `npx webpack --env target=web --mode development`;
    - run `git diff --check` and `git diff --cached --check`.
11. Update this document with:
    - the observed `logUri` shape;
    - the final derivation used to find `exthost.log`;
    - explicit statement that `AGDA_TEST_USER_DATA_DIR` was removed;
    - focused reproduction result;
    - full-suite result;
    - build/check results.
12. Cleanup the lab:
    - remove temporary path-shape logging/assertion that is not part of the
      permanent helper;
    - leave `test/tests/assets/Goals.agda` and all other fixtures unmodified;
    - leave the working tree containing only intentional source/test/docs and
      generated-output changes.

### H29 Results (2026-07-10)

Step 1 baseline: `AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm
test` reproduced before any change (1 passing).

Changes made, exactly per the handoff:

- `test/RunTestFromCLI.res`: removed the
  `Dict.set("AGDA_TEST_USER_DATA_DIR", testUserDataDir)` call and its
  explanatory comment. The fresh `--user-data-dir` / `--extensions-dir`
  launch args are unchanged.
- `src/Main/Main.res`: `activationExports` gained a `logUri: VSCode.Uri.t`
  field; `activate` now reads `VSCode.ExtensionContext.logUri(context)` and
  returns it alongside `channels`/`memento`.
- `test/tests/Test__Util.res`: added `activatedLogUri: ref<option<VSCode.Uri.t>>`,
  set from `exports.logUri` inside `activateExtension`. Rewrote
  `ExtHostLog.locate` to fail loudly if the extension hasn't been activated
  yet, otherwise derive `exthost.log` as
  `NodeJs.Path.join([NodeJs.Path.dirname(VSCode.Uri.fsPath(logUri)), "exthost.log"])`,
  failing loudly (with the observed `logUri.fsPath` and candidate path) if
  that exact file doesn't exist. No `/tmp` globbing, no environment variable,
  no sidecar file.

Observed `logUri` shape (captured via a temporary `Js.log` in `Main.activate`,
removed after confirming; required a webpack rebuild to take effect, since
the running extension host loads `dist/app.bundle.js`, not `lib/js` directly
-- an initial focused run after only `rescript build` failed with `Cannot
read properties of undefined (reading 'fsPath')` because the bundle still
had the pre-H29 `activate`, `exports.logUri` was therefore `undefined`):

```
<user-data>/logs/<timestamp>/window1/exthost/banacorn.agda-mode
```

matching exactly the expected shape from the handoff, with `exthost.log` as
the sibling in the same `exthost` directory. `AGDA_TEST_USER_DATA_DIR` was
confirmed removed; a repo-wide grep for `AGDA_TEST_USER_DATA_DIR`,
`AGDA_ISSUE300_USER_DATA_DIR`, `AGDA_ISSUE300_PROBE`, `Issue300Harness`, and
`Issue300Probe` across `src`/`test` (source and generated) returned nothing.

Verification:

- focused reproduction: 3/3 independent `npm test` runs passing (308ms /
  467ms / and one earlier run at 315ms during the shape investigation), each
  with `test/tests/assets/Goals.agda` confirmed unmodified afterward.
- full `npm test`: 858 passing, 0 failing (36s).
- `npx rescript build`, `npx webpack --mode development`, and
  `npx webpack --env target=web --mode development` all exit 0.
- `git diff --check` and `git diff --cached --check` both exit 0 after
  staging (one round of the recurring ReScript-codegen trailing-whitespace
  cleanup was needed on `lib/js/src/Main/Main.bs.js` and
  `lib/js/test/tests/Test__Util.bs.js`, via `sed -i '' 's/ *$//'`, verified
  with `node -c` before restaging).

Working tree after this round contains only: `docs/issue#300.md`,
`src/Main/Main.res` (+ compiled), `test/RunTestFromCLI.res` (+ compiled),
`test/tests/Test__Util.res` (+ compiled). No other files changed;
`test/tests/Test__StaleEditorDecorationWarning.res` itself needed no edits.

### H30: fix stale captured-editor listener routing

Hypothesis: the retained #300 reproducer fails because `Main.initialize`
registers per-state global listeners that process events for every document
with the `editor` captured when that state was initialized. Filtering each
listener to its own document and using the event/current editor for the
matching event should stop VS Code from receiving `setDecorations` calls on
the stale captured `TextEditor`, without changing the reproduction sequence.

This is the first fix handoff. Do not search for a new reproduction. Do not add
manual UI steps. Do not add a new issue-specific harness, probe, environment
variable, or H-numbered test file. Do not catch, suppress, or filter the VS Code
warning in tests or logs. The fix must make the warning stop by preventing the
stale-editor call path.

Lab assistant task:

1. Start on the current `issue#300-new` branch after H29. Confirm the baseline
   reproducer still passes as a reproducer before changing code:
   ```sh
   AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test
   ```
   Record the result here. If it does not pass, stop and record the failure;
   do not start the fix from a broken baseline.
2. In `src/Main/Main.res`, make the
   `VSCode.Window.onDidChangeTextEditorSelection` listener document-scoped:
   - get the event editor with
     `VSCode.TextEditorSelectionChangeEvent.textEditor(event)`;
   - get its document/file name from that event editor;
   - compare it to the `id` computed at the top of `initialize`;
   - if it does not match, return without calling
     `State__InputMethod.select`;
   - if it matches, compute selection offsets against the event editor's
     document, not against the initialization-time captured `editor`.
3. In `src/Main/Main.res`, make the
   `VSCode.Workspace.onDidChangeTextDocument` listener document-scoped:
   - get the event document with
     `VSCode.TextDocumentChangeEvent.document(event)`;
   - compare its file name to the same `id`;
   - if it does not match, return without calling `IM.Input`,
     `State__InputMethod.keyUpdateEditorIM`, `Tokens.applyEdit`, or
     `Goals.scanAllGoals`;
   - if it matches, read the current state editor (`state.editor`) and verify
     that its document file name still equals `id`;
   - if the current state editor does not match `id`, return without doing the
     editor-dependent calls for this event;
   - if it matches, use that current state editor for the existing
     editor-based calls instead of the initialization-time captured `editor`.
4. Keep the existing listener behavior for matching-document events:
   - preserve `IM.Input.fromTextDocumentChangeEvent`;
   - preserve `State__InputMethod.keyUpdateEditorIM`;
   - preserve `Tokens.applyEdit`;
   - preserve goal-change mapping and `Goals.scanAllGoals`;
   - preserve the existing subscription structure.
5. Do not change `src/Editor.res` to swallow `setDecorations` warnings. The
   product fix belongs in event routing / editor selection in `Main.initialize`.
6. Rebuild generated ReScript output after editing source:
   ```sh
   npx rescript build
   ```
   Then rebuild the desktop extension bundle before any extension-host test
   run, because `npm test` loads `dist/app.bundle.js`, not `lib/js` directly:
   ```sh
   npx webpack --mode development
   ```
7. Convert `test/tests/Test__StaleEditorDecorationWarning.res` from a
   reproducer into a regression test:
   - keep the same file name;
   - keep the same describe block;
   - keep the same open/load/tab-switch/type/delete sequence;
   - update comments and the `it` title so they say the sequence no longer
     produces the exact warning;
   - change the assertion from `Assert.ok(after - before > 0)` to an exact
     zero-delta assertion, e.g. `Assert.equal(0, after - before)`.
8. Rebuild again if the test source changed, and rebuild the desktop bundle
   again if `src/Main/Main.res` changed after step 6:
   ```sh
   npx rescript build
   npx webpack --mode development
   ```
9. Verify the focused regression:
   ```sh
   AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test
   ```
   The test must pass only because `after - before == 0`. If warnings still
   increase, record the before/after counts and stop; do not weaken the
   assertion.
10. Verify normal coverage and bundles:
    ```sh
    npm test
    npx webpack --mode development
    npx webpack --env target=web --mode development
    git diff --check
    git diff --cached --check
    ```
11. Search for accidental reintroduced cleanup targets:
    ```sh
    rg "AGDA_TEST_USER_DATA_DIR|AGDA_ISSUE300_USER_DATA_DIR|AGDA_ISSUE300_PROBE|Issue300Harness|Issue300Probe" src test lib/js
    ```
    Expected: no matches in source/test/generated code. Historical matches in
    this document are okay and should not be removed just to satisfy this
    source/test check.
12. Update this section with:
    - baseline focused reproducer result before the fix;
    - files changed;
    - the exact routing guard implemented in both listeners;
    - focused regression result and observed warning delta;
    - full `npm test` result;
    - build/bundle/check results;
    - any transient failure and its rerun result.
13. Cleanup the lab:
    - remove temporary logging or diagnostics;
    - leave `test/tests/assets/Goals.agda` and other fixtures unmodified;
    - leave only intentional source/test/docs and generated-output changes in
      the working tree.

### H30 Results (2026-07-11)

Executed as specified; the fix works and the reproducer is now a regression
test.

- **Baseline before the fix**: the focused reproducer passed as a reproducer
  (`AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test`,
  1 passing, `after - before > 0` held), confirming a valid starting point.
- **Files changed**:
  - `src/Main/Main.res` — routing guards in both listeners;
  - `test/tests/Test__StaleEditorDecorationWarning.res` — inverted into the
    regression test;
  - generated output (`lib/js/src/Main/Main.bs.js`,
    `lib/js/test/tests/Test__StaleEditorDecorationWarning.bs.js`).
    Recompilation also produced whitespace-only churn in
    `lib/js/test/tests/Test__Util.bs.js`; that file was reverted to its
    committed state since it has no H30 change.
- **Routing guard implemented**:
  - `onDidChangeTextEditorSelection`: reads the event editor via
    `VSCode.TextEditorSelectionChangeEvent.textEditor(event)`, compares its
    document file name to `id`, returns without calling
    `State__InputMethod.select` on mismatch, and computes selection offsets
    against the event editor's document instead of the captured `editor`.
  - `onDidChangeTextDocument`: reads the event document via
    `VSCode.TextDocumentChangeEvent.document(event)`, returns on file-name
    mismatch with `id`; on match, reads `state.editor`, verifies its document
    file name still equals `id`, returns if not, and otherwise uses that
    current editor for `IM.Input.fromTextDocumentChangeEvent`,
    `Tokens.applyEdit`, and `Goals.scanAllGoals`. All matching-document
    behavior and the subscription structure are preserved; `src/Editor.res`
    is untouched.
- **Focused regression after the fix**: passes with the exact zero-delta
  assertion `Assert.equal(0, after - before)` — observed warning delta 0.
  The `it` title is now "a text edit after staling the captured editor
  produces no warning".
- **Full `npm test`**: 858 passing, 8 pending, 0 failing, exit 0.
- **Builds/checks**: `npx rescript build` clean; desktop bundle
  (`npx webpack --mode development`) and web bundle
  (`npx webpack --env target=web --mode development`) both compiled
  successfully. The ReScript compiler emits trailing whitespace in generated
  `.bs.js` files; it was stripped from the two changed generated files, after
  which both `git diff --check` and `git diff --cached --check` pass clean.
- **Cleanup-target search**: `rg "AGDA_TEST_USER_DATA_DIR|
  AGDA_ISSUE300_USER_DATA_DIR|AGDA_ISSUE300_PROBE|Issue300Harness|
  Issue300Probe" src test lib/js` — no matches.
- **Transient failures**: none; every step passed on its first run.
- **Cleanup**: no temporary logging or diagnostics were added;
  `test/tests/assets/Goals.agda` and other fixtures are unmodified; the
  working tree contains only the intentional source/test/docs and
  generated-output changes listed above.

## Relevant locations

| File | What to look at |
|------|-----------------|
| `src/Main/Main.res:44` | `initialize` - registers per-file global listeners |
| `src/Main/Main.res:114` | `onDidChangeTextEditorSelection` - gated by the event editor's document (H30); computes offsets against the event editor's document, ignores other documents' events |
| `src/Main/Main.res:130-156` | `onDidChangeTextDocument` - gated by the event document (H30); verifies `state.editor` still points at this document and uses it, never the initialization-time captured `editor`, for editor-dependent calls |
| `test/tests/Test__StaleEditorDecorationWarning.res` | #300 regression test (inverted from the reproducer by H30); same sequence, asserts a zero warning delta |
| `test/tests/Test__Util.res` | `ExtHostLog` helper derives `exthost.log` from `ExtensionContext.logUri` |
| `test/TestSuiteAdapter.res` | `AGDA_TEST_GLOB` focused test-file selection |
| `docs/im-performance-investigation.md` | #318 investigation; separate `Tokens.deltas` accumulation mechanism |
