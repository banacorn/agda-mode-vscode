# Input Method Performance Investigation

## Summary

The input method slowdown was caused by token edit history accumulating for the lifetime of an Agda load. Every text edit appended or updated entries in `Tokens.deltas`, and every highlighting pass walked that growing structure. Repeated IM use made the list grow linearly, so later keystrokes paid more bookkeeping cost than earlier keystrokes.

The implemented fix consumes `deltas` after each edit: tokens and holes are rebased into current VSCode offsets, removed tokens are dropped, and `deltas` is reset to `TokenIntervals.empty`.

## Symptom

Users reported that the input method got progressively slower within a single session. A fresh session felt responsive, but repeated IM activations such as typing `\`, entering an IM sequence, and accepting the replacement became increasingly laggy before the file was loaded again.

The IM itself was not the slow path. The slowdown came from token position tracking that runs after every `onDidChangeTextDocument` event.

## Root Cause

`Tokens.applyEdit` used `TokenIntervals.applyChanges` to maintain a linked list of edit intervals:

```rescript
type rec t =
  | EOF
  | Replace(int, int, int, t) // start, end, cumulative delta, tail
```

Before the fix, this list was only cleared on `Tokens.reset`, which happens during Agda load/clear-highlighting. Ordinary edits and IM edits between loads kept accumulating interval nodes.

Each IM use can produce more than one document change:

1. The user's raw keystroke.
2. The IM rewrite that replaces the typed sequence with the selected symbol.

After many IM uses, `generateHighlighting` was still traversing all historical intervals even though only the current token positions mattered.

## Reproduction Guard

The regression test in `test/tests/Test__EditorIM.res` runs five identical IM sessions and records `Tokens.deltasLength` after each one. Before rebasing, the failure shape was linear growth:

```text
actual:   [2, 3, 4, 5, 6]
expected: [2, 2, 2, 2, 2]
```

After the fix, `deltasLength` remains stable because each edit consumes the interval state immediately.

## Implemented Fix

The fix lives in `src/Tokens.res`.

Current invariant after each edit:

- `agdaTokens` stores current VSCode UTF-16 offsets.
- `holes` is keyed by current VSCode UTF-16 offsets.
- same-file `token.source` offsets are current VSCode UTF-16 offsets.
- cross-file `token.source` offsets remain Agda 1-based offsets and are converted when the source file is opened.
- `deltas == TokenIntervals.empty`.

`applyEdit` now:

1. Builds `self.deltas` from the incoming content changes.
2. Calls `rebaseTokens(self, editingFilepath)`.
3. Calls `generateHighlighting`.

`rebaseTokens` consumes the interval list once:

- `Remove` tokens are omitted from the rebuilt tree.
- `Translate(delta)` tokens are inserted with `start + delta` and `end + delta`.
- same-file source offsets are translated with the same VSCode-offset delta.
- holes are rebuilt from the translated tokens.
- `self.deltas` is cleared after rebasing.

This prevents double-applying deltas: `generateHighlighting`, `toTokenArray`, and same-file `goToDefinition` now see already-current offsets.

## Source Offset Handling

Token starts and ends are converted from Agda offsets to VSCode offsets during `insertTokens`.

Source offsets need a split representation:

- Same-file definitions are converted to VSCode offsets at load time so they can be rebased with local edits.
- Cross-file definitions keep Agda offsets because they must be converted with the source file's text, not the current editor's text.

`lookupSrcLoc` branches on the stored filepath:

- same file: use the stored VSCode offset directly.
- cross file: open the source document, build an `Agda.OffsetConverter` from that document's text, then convert the Agda offset.

This matters for files containing CRLF or non-BMP Unicode, where Agda offsets and VSCode UTF-16 offsets are not interchangeable.

## Regression Guards

The guard suite covers the behavior that made this change risky:

| Guard | Test file | Purpose | Current expectation |
|------:|-----------|---------|---------------------|
| 1 | `test/tests/Test__EditorIM.res` | Repeated IM sessions must not grow `deltasLength`. | Passes. |
| 2 | `test/tests/Test__TokenBookkeeping.res` | Semantic-token positions stay correct after repeated ordinary inserts. | Passes. |
| 3 | `test/tests/Test__TokenBookkeeping.res` | Deleted highlighted tokens are removed and remaining positions stay correct. | Passes. |
| 4 | `test/tests/Test__Tokens.res` | Hole start/end offsets stay correct after repeated edits before the hole. | Passes. |
| 5 | `test/tests/Test__Tokens.res` | Go-to-definition source ranges stay correct after a positive-delta edit before the referenced token. | Passes. |
| 6 | `test/tests/Test__TokenBookkeeping.res` | `Command.Load` clears deltas and does not retain old decorations. | Passes. |
| 7 | `test/tests/Test__Tokens.res` | `Tokens.toTokenArray` reports current positions after positive-delta edits. | Passes. |
| 8 | `test/tests/Test__TokenBookkeeping.res` | Decoration ranges stay correct after ordinary edits. | Passes. |
| 9 | `test/tests/Test__Tokens.res` | `Tokens.toTokenArray` and go-to-definition handle negative-delta edits. | Passes. |
| 10 | `test/tests/Test__Tokens.res` | Cross-file go-to-definition uses the source file's non-BMP Unicode offset conversion. | Passes. |
| 11 | `test/tests/Test__Tokens.res` | Cross-file go-to-definition uses the source file's CRLF offset conversion. | Passes. |

`Tokens.deltasLength` remains useful as a regression probe and should not be removed without replacing the IM accumulation guard.

## Verification

The rebasing implementation has been checked with:

```sh
git diff --check
npx rescript
TMPDIR=/tmp npm test
```

The full suite passes with `856 passing` and `8 pending`.

## Relevant Files

| File | What to look at |
|------|-----------------|
| `src/Tokens.res` | `insertTokens`, `lookupSrcLoc`, `rebaseTokens`, and `applyEdit`. |
| `src/Tokens/Token.res` | Documents the mixed source-offset representation. |
| `src/Tokens/TokenIntervals.res` | Interval representation and `applyChanges`. |
| `test/tests/Test__EditorIM.res` | IM accumulation regression guard. |
| `test/tests/Test__TokenBookkeeping.res` | Semantic token, decoration, and load-reset guards. |
| `test/tests/Test__Tokens.res` | Hole, token tree, and go-to-definition guards. |
