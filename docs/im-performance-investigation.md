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

The test **fails** with linear growth:

```
AssertionError: Expected values to be strictly deep-equal:

actual:   [ 2, 3, 4, 5, 6 ]  // or [3, 4, 5, 6, 7], depending on earlier edits in the suite
expected: [ 2, 2, 2, 2, 2 ]  // or the same first value repeated
```

`deltasLength` increases by one with every IM session, even though each session types the same two characters. No Agda reload (`ClearHighlighting`) happened between sessions, so the edit history is never reset. Since `generateHighlighting` traverses the full list on every subsequent keystroke, a session that has gone through twenty IM uses is processing a list ten times longer than one that has gone through two — which is exactly the "gradually getting slower" shape users are reporting.

## Regression guard baseline

The guard suite is now in place. It locks down both behavior that must continue to work during rebasing and behavior that the rebasing fix is expected to correct.

Current pre-rebase baseline:

- `npm test` has exactly four intentional failures: Guards 1, 5, 7, and 9.
- Guards 2, 3, 4, 6, and 8 pass and must stay green.
- After rebasing, all guards should pass.
- Do not weaken `Tokens.deltasLength` or replace concrete position/range assertions with weaker non-empty checks.

| Guard | Test file | Purpose | Pre-rebase expectation |
|------:|-----------|---------|------------------------|
| 1 | `test/tests/Test__EditorIM.res` | Reproduce IM `deltas` accumulation across repeated IM sessions. | Fails with linear `deltasLength` growth. |
| 2 | `test/tests/Test__TokenBookkeeping.res` | Keep semantic-token positions correct after repeated ordinary inserts. | Passes. |
| 3 | `test/tests/Test__TokenBookkeeping.res` | Remove deleted highlighted tokens and keep remaining token positions correct. | Passes. |
| 4 | `test/tests/Test__Tokens.res` | Keep hole start/end offsets correct after repeated edits before the hole. | Passes. |
| 5 | `test/tests/Test__Tokens.res` | Keep go-to-definition source ranges correct after a positive-delta edit before the referenced token. | Fails: source range is stale by one column. |
| 6 | `test/tests/Test__TokenBookkeeping.res` | Confirm `Command.Load` clears deltas and does not retain old decorations. | Passes. |
| 7 | `test/tests/Test__Tokens.res` | Ensure `Tokens.toTokenArray` reports current positions after positive-delta edits. | Fails: the `data` token maps to `(1,0)` instead of `(3,0)`. |
| 8 | `test/tests/Test__TokenBookkeeping.res` | Keep decoration ranges correct after ordinary edits. | Passes. |
| 9 | `test/tests/Test__Tokens.res` | Ensure `Tokens.toTokenArray` and go-to-definition handle negative-delta edits. | Fails: the moved `f` token maps to `(11,27)` instead of `(11,26)`. |

## Handoff: implement rebasing

Goal: stop `deltas` from accumulating across ordinary edits and IM edits while preserving every visible token, decoration, hole, and go-to-definition position.

Recommended invariant after each edit:

- `agdaTokens` are stored in current VSCode offsets.
- `holes` is keyed by current VSCode offsets.
- `deltas == TokenIntervals.empty`.
- `generateHighlighting`, `toTokenArray`, and `goToDefinition` read current offsets and do not apply already-consumed deltas again.

Suggested implementation shape:

1. Add a small helper near `traverseIntervals`, for example `rebaseTokens(self)`.
2. In `applyEdit`, keep the existing `TokenIntervals.applyChanges` call to incorporate the latest document changes.
3. Immediately call `rebaseTokens(self)` before `generateHighlighting`.
4. In `rebaseTokens`, traverse `self->toTokenArray` with `self.deltas`.
5. For `Remove`, omit the token from the rebuilt tree.
6. For `Translate(delta)`, insert `{...token, start: token.start + delta, end: token.end + delta}` into a fresh `agdaTokens` tree.
7. Rebuild `self.holes` from the translated tokens at the same time, using translated starts as keys.
8. Set `self.deltas = TokenIntervals.empty` after rebuilding tokens and holes.
9. Then call `generateHighlighting(self, editor)`.

Main risk: double-applying deltas. If tokens are rebased to current offsets but `deltas` is left non-empty, `generateHighlighting` will shift the same edit again. If `deltas` is cleared before tokens are rebased, removed tokens and shifted ranges can be lost. Keep the boundary explicit: consume `deltas` once, rebuild current-offset token state, then clear `deltas`.

Expected result:

- Guards 1, 5, 7, and 9 turn green.
- Guards 2, 3, 4, 6, and 8 stay green.
- `npm test` passes.
- `Tokens.deltasLength` remains available and meaningful as a regression probe; do not delete or weaken it.

## Relevant files

| File | What to look at |
|------|-----------------|
| `src/Tokens.res:463–472` | `applyEdit` — calls `generateHighlighting` synchronously after every change |
| `src/Tokens/TokenIntervals.res:217` | `applyChangeAux` — the recursive function that grows the list |
| `src/Tokens.res:249–252` | `reset` — the only place `deltas` is cleared (on Agda load) |
| `src/Main/Main.res:126–143` | The `onDidChangeTextDocument` handler wiring it all together |
| `src/InputMethod/IM.res:337–391` | `applyRewrites` — where the IM's second `batchReplace` event originates |
