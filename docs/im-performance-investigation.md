# Input Method Performance Investigation

## Symptom

Users report the input method (IM) gets progressively slower within a session. The degradation is noticeable after repeated IM use: activating the IM with `\`, typing a sequence like `\lambda`, and seeing `λ` appear feels responsive at first, but subsequent uses of the IM in the same session feel laggier each time.

## Background: what happens on every keystroke

When the user types while an Agda file is open, `onDidChangeTextDocument` fires (`src/Main/Main.res:126`). For every such event the extension runs three things in sequence:

1. **IM processing** — `State__InputMethod.keyUpdateEditorIM` detects whether the keystroke is part of an active IM sequence and schedules a text replacement if needed.
2. **Token position tracking** — `Tokens.applyEdit` (`src/Tokens.res:463`) updates an internal structure called `deltas` that maps original Agda token positions to their current positions after user edits, then immediately calls `generateHighlighting`.
3. **Goal position tracking** — `Goals.scanAllGoals` (`src/Goals.res:684`) walks all open holes to keep their positions correct.

The IM itself (step 1) is fast. The slowdown is coming from step 2.

## The mechanism: `deltas` accumulates across IM uses

`Tokens.applyEdit` maintains a linked list called `deltas` (type `TokenIntervals.t`, defined in `src/Tokens/TokenIntervals.res`):

```rescript
type rec t =
  | EOF
  | Replace(int, int, int, t)  // start, end, cumulative delta, tail
```

Each call to `TokenIntervals.applyChanges` either modifies existing nodes or **appends new `Replace` nodes** for changes in previously-untouched regions. The list only resets when Agda reloads the file (`ClearHighlighting` response, `src/Tokens.res:252`).

Every IM keystroke produces **two** `onDidChangeTextDocument` events, each growing `deltas`:

1. The user's raw keystroke (e.g. inserting `l` into `\l`)
2. The IM's own `batchReplace` rewriting the partial sequence (e.g. replacing `\l` with `←`)

Both events call `applyChanges`, potentially adding nodes to the list. After N IM uses between loads, the list has accumulated N × (nodes per use) entries. Every subsequent call to `generateHighlighting` — which traverses the full list — is proportionally slower.

## Reproduction

A `deltasLength` function was added to expose the internal node count:

- `src/Tokens/TokenIntervals.res` — `let rec length`
- `src/Tokens.res` — `let deltasLength: t => int`

The test in `test/tests/Test__EditorIM.res` ("Input Method — deltas growth regression") runs five back-to-back IM sessions. Each session does exactly the same work: activate the IM, type `b` then `n` (which translates to `𝕟` and auto-deactivates), then record `deltasLength`. The assertion is that all five values are equal.

The test **fails**:

```
AssertionError: Expected values to be strictly deep-equal:

actual:   [ 2, 3, 4, 5, 6 ]
expected: [ 2, 2, 2, 2, 2 ]
```

`deltasLength` increases by one with every IM session, even though each session types the same two characters. No Agda reload (`ClearHighlighting`) happened between sessions, so the edit history is never reset. Since `generateHighlighting` traverses the full list on every subsequent keystroke, a session that has gone through twenty IM uses is processing a list ten times longer than one that has gone through two — which is exactly the "gradually getting slower" shape users are reporting.

## Handoff: regression guards before rebasing

Before implementing any rebasing, add tests that lock down the observable behavior that `Tokens.applyEdit` and `generateHighlighting` already provide. The fix should change how much stale edit history is retained, not what users see after edits.

### Executor checklist

The executor/coder owns this checklist. Update it while working: use `[ ]` for not started, `[~]` for in progress, and `[x]` for done. Keep status notes short but concrete: test filename, test name, and observed pass/fail status.

- [ ] **Guard 1: keep the IM accumulation reproducer**
  - File: `test/tests/Test__EditorIM.res`
  - Status note:
  - Test shape: run five consecutive IM sessions in one editor session. Each session activates the IM, types `b`, types `n`, and records `Tokens.deltasLength`.
  - Current-code expectation: this test fails with actual `[2, 3, 4, 5, 6]` vs expected `[2, 2, 2, 2, 2]`.
  - Post-fix expectation: this test passes because `deltas` no longer accumulates across IM sessions.

- [ ] **Guard 2: semantic-token and decoration positions after repeated ordinary edits**
  - File: `test/tests/Test__TokenBookkeeping.res` or `test/tests/Test__Tokens.res`
  - Status note:
  - Test shape: load an existing fixture such as `Issue180.agda`, make one edit before known highlighted tokens, wait for token update, assert `Highlighting__SemanticToken.toString` output, then make a second edit before the same region and assert translated token output again. If the fixture produces decorations, also assert `Tokens.toDecorations` ranges after both edits.
  - Current-code expectation: this guard passes before rebasing starts.
  - Post-fix expectation: this guard still passes.

- [ ] **Guard 3: token removal after deleting highlighted source**
  - File: `test/tests/Test__TokenBookkeeping.res` or `test/tests/Test__Tokens.res`
  - Status note:
  - Test shape: load a fixture with known highlighted tokens, delete a line or range containing those tokens, assert removed tokens no longer appear in `Tokens.getVSCodeTokens`, then make another edit after the deletion and assert remaining tokens still have correct positions.
  - Current-code expectation: this guard passes before rebasing starts.
  - Post-fix expectation: this guard still passes.

- [ ] **Guard 4: hole positions after repeated edits**
  - File: `test/tests/Test__Tokens.res`, `test/tests/Test__TokenBookkeeping.res`, or an existing goal-position test file if that fits better.
  - Status note:
  - Test shape: load a fixture with a hole, such as `GotoDefinition.agda` or another existing goal fixture; make an edit before the hole; read `Tokens.getHolePositionsFromLoad`; assert the hole start/end offsets moved to current editor positions; make a second edit before the hole and assert the positions move exactly once again.
  - Current-code expectation: this guard passes before rebasing starts.
  - Post-fix expectation: this guard still passes.

- [ ] **Guard 5: go-to-definition after edits before the referenced token**
  - File: `test/tests/Test__Tokens.res`
  - Status note:
  - Test shape: extend the existing `goToDefinition` coverage. Load `Lib.agda`, insert text before the token used by the current go-to-definition test, call `Tokens.goToDefinition` at the token's new editor position, and assert it still returns the same source filepath and definition position. Also assert the returned source range is at the token's new editor range.
  - Current-code expectation: this guard passes before rebasing starts.
  - Post-fix expectation: this guard still passes.

- [ ] **Guard 6: load/reset behavior**
  - File: `test/tests/Test__TokenBookkeeping.res`
  - Status note:
  - Test shape: keep or extend the existing `Command.Load` test. Confirm `Command.Load` still clears old deltas via `ClearHighlighting`, highlighting after a load is based on fresh Agda tokens, and decorations from a previous load do not survive into the next load.
  - Current-code expectation: this guard passes before rebasing starts.
  - Post-fix expectation: this guard still passes.

- [ ] **Guard 7: token tree structure after rebasing-sensitive edits**
  - File: `test/tests/Test__Tokens.res`
  - Status note:
  - Test shape: load a fixture with multiple tokens on the same and following lines; make two edits before those tokens; read `Tokens.toTokenArray`; assert token starts are sorted, unique, and match current editor positions; assert `Tokens.goToDefinition` still finds a token whose start moved because of those edits.
  - Current-code expectation: this guard passes before rebasing starts.
  - Post-fix expectation: this guard still passes.

### Reviewer/investigator checklist

The reviewer/investigator owns this checklist. The executor/coder should not mark these items complete; it should leave enough test output and status notes for the reviewer to evaluate them.

- [ ] **Baseline review before rebasing**
  - `npm test` should have exactly one expected failure: the IM accumulation reproducer.
  - All newly added guards except the IM accumulation reproducer should pass on current code.
  - If any non-IM guard fails before rebasing, fix the guard or explain why the current behavior is already broken before proceeding.

- [ ] **Post-rebase review**
  - `npm test` should pass.
  - The IM accumulation reproducer must pass without deleting or weakening `Tokens.deltasLength`.
  - The non-IM guards must still assert concrete positions/ranges, not only non-empty outputs.

## Relevant files

| File | What to look at |
|------|-----------------|
| `src/Tokens.res:463–472` | `applyEdit` — calls `generateHighlighting` synchronously after every change |
| `src/Tokens/TokenIntervals.res:217` | `applyChangeAux` — the recursive function that grows the list |
| `src/Tokens.res:249–252` | `reset` — the only place `deltas` is cleared (on Agda load) |
| `src/Main/Main.res:126–143` | The `onDidChangeTextDocument` handler wiring it all together |
| `src/InputMethod/IM.res:337–391` | `applyRewrites` — where the IM's second `batchReplace` event originates |
