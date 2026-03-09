# Connection System Specification

## Purpose and Scope

This spec defines the normative behavior of the connection system, covering:
- **Chain ordering** — the sequence and conditions under which connection candidates are tried
- **State updates** — how `connection.paths` and `PickedConnection` are read and written
- **UI effects** — what the Switch Version UI exposes and what actions it performs

This spec does not cover:
- Download protocols or network implementation details
- Platform-specific behavior (desktop vs web)
- Internal storage formats (e.g. memento key layout)
- Version probing internals

## Normative Rules

**Chain ordering**
- The chain MUST be tried in step order (0 → 1 → 2 → 3 → 4), stopping at the first success.
- Failure at any step MUST NOT abort the chain; the chain MUST continue to the next step.
- Step 0 MUST try `PickedConnection` if set, regardless of whether it appears in `connection.paths`.
- Step 1 MUST exclude any entry that exactly matches `PickedConnection` (by string) to avoid double probing.
- Step 2 MUST skip `agda` (or `als`) if the exact string `"agda"` (or `"als"`) appears in `connection.paths` or equals `PickedConnection`.

**`connection.paths` updates**
- Command-discovered paths (step 2) MUST NOT be added to `connection.paths`.
- When a download completes (chain or UI-triggered), the downloaded path MUST be appended to `connection.paths`, deduplicated.

**`PickedConnection` updates**
- `PickedConnection` MAY be an absolute path or a bare command name, matching whatever the user selected.
- `PickedConnection` MUST only be changed by:
  - step 2 success — set to the bare command name (e.g. `"agda"`), not the resolved path
  - a download completing — set to the downloaded path
  - explicit endpoint selection in the Switch Version UI — set to the selected path/command
- `PickedConnection` MUST NOT be auto-cleared or demoted on failure.
- Endpoint selection in the Switch Version UI MUST NOT add the selected path to `connection.paths`.

**Switch Version UI**
- The channel selection UI MUST be hidden when fewer than two channels are available.

## The Connection Chain

The chain is tried in order, stopping at the first success. Failure at any step is non-fatal — the chain continues to the next step.

0. **Picked** — `PickedConnection` if set, regardless of whether it appears in `connection.paths`
1. **Paths** — remaining entries in `agdaMode.connection.paths` (excluding any entry that exactly matches `PickedConnection`)
2. **Commands** — `agda`, `als` looked up from PATH (skipped individually if the exact string `"agda"` or `"als"` appears in `connection.paths` or equals `PickedConnection`)
3. **Download native** — download native binary via the active channel
4. **Download WASM** — fallback if native download fails **or** native channel resolution fails

## Channels

A channel determines which version of ALS to download. The format fallback (native → WASM) is orthogonal to channel selection: regardless of which channel is active, native is always tried before WASM.

- `Hardcoded` — temporary pinned known-good version *(current)*
- `LatestALS` — latest release from GitHub *(planned)*
- `DevALS` — dev release *(planned)*


## `connection.paths`

`agdaMode.connection.paths` is a list of paths or command names tried at step 1 of the chain. It is managed primarily by the user, but the system also appends to it when a download completes. Entries can be absolute paths (e.g. `/usr/local/bin/agda`) or bare command names (e.g. `agda`) that are resolved from PATH on each run.

The list is probed in **reverse order** — the last entry is tried first. This is intentional and documented in the config schema (`package.json`: "from the LAST to the first").

When the chain downloads an ALS binary (steps 3–4), the downloaded path is appended to `connection.paths` (deduplicated). Since `getAgdaPaths()` reverses the list before probing, the last entry is tried first — so an appended download naturally has the highest priority even without `PickedConnection` (see `package.json` config schema).

Command-discovered paths (step 2) are **never** added to `connection.paths`. If a user puts `agda` in the list, they intend for it to resolve from PATH on every run. Adding the resolved absolute path back to the list would override that intent (see PR #272).

## `PickedConnection`

`PickedConnection` (stored in memento) records the user's preferred connection path (see step 0 of the chain).

**Lifecycle rules:**
- `PickedConnection` is sticky and must not be auto-cleared or demoted on connection failure.
- `PickedConnection` changes only via step 2 command success, an explicit user action (Switch Version UI), or a download action.

`PickedConnection` is set in four ways:

1. **Command lookup (step 2)** — when `agda` or `als` is found in PATH and used to connect, `PickedConnection` is set to the bare command name (e.g. `"agda"`), not the resolved absolute path.
2. **Chain auto-download** — when the chain downloads ALS as a fallback (steps 3–4), it sets both `connection.paths` and `PickedConnection`.
3. **UI download** — when the user triggers a download from the Switch Version UI, same result.
4. **UI endpoint selection** — when the user selects an entry (from `connection.paths` or PATH-discovered), only `PickedConnection` is updated. The path is never added to `connection.paths` here.

## Switch Version UI

The UI is a VSCode QuickPick. It has three sections, always in this order:

### 1. Endpoints

Lists all available connection endpoints in a single merged list:
- Entries from `connection.paths` first (user's saved list, in stored order)
- PATH-discovered commands (`agda`, `als`) appended — **only when actually found in PATH**

The entry whose path/command exactly matches `PickedConnection` is marked with a checkmark. If `PickedConnection` is `None`, nothing is marked.

If no endpoints exist (empty `connection.paths` and no PATH commands found), a "No installations" item is shown instead.

**Selecting an endpoint** sets `PickedConnection` to the selected path/command. `connection.paths` is never modified by endpoint selection.

### 2. Channel (hidden when fewer than 2 channels are available)

Currently hidden — only `Hardcoded` exists, so the section does not appear.

When LatestALS/DevALS are restored, this section lists all available channels: `Hardcoded`, `LatestALS`, `DevALS` (DevALS only when DevMode is enabled). The active channel is marked.

**Selecting a channel** immediately triggers a download for that channel. On success, the downloaded path is appended to `connection.paths` and `PickedConnection` is set to it.

### 3. Misc

- **Delete Downloads** — clears ALL downloaded ALS from global storage (every channel).

### Download section (absent)

There is no separate "Download" section. Downloads happen automatically via the chain (steps 3–4), or via channel selection (above) when multiple channels are available.

## Implementation Issues

- Step-2 `PickedConnection` update is not yet aligned:
  - Spec now requires setting `PickedConnection` to the bare command name on step-2 success.
  - Existing behavior/tests still assume step-2 does not update `PickedConnection`.

## Testing to Add/Fix

This section tracks the current status of test alignment with this spec.

### Current Status (2026-03-09)

- Full suite is green: `npm test` reports **591 passing, 8 pending, 0 failing**.
- Connection contract coverage in `test/tests/Test__Connection.res` is green:
  - `"should try PickedConnection first even when it is not in connection.paths"`
  - `"should continue to later steps when PickedConnection fails"`
  - `"should not re-probe PickedConnection in the paths step"`
  - `"should skip agda/als command probes when already present in connection.paths"`
  - `"should skip agda command probe in step 2 when PickedConnection is bare agda"`
  - `"should not persist resolved absolute command paths back into connection.paths"`
  - `"should update both connection.paths and PickedConnection after successful download"`
  - `"should retry Hardcoded download with WASM source when native download fails"`
  - `"should retry Hardcoded download with WASM source when Hardcoded channel resolution fails"`
  - `"should fall back to WASM when Hardcoded native download fails"`

### Completed

- Download failure contracts in `test/tests/Test__Connection.res` are fixed and passing.
- Switch Version UI selection/download config semantics in `test/tests/Connection/Test__Connection__Config.res` are fixed and passing.
- Bare-command endpoint selection (`"agda"` / `"als"`) now preserves raw selection in `PickedConnection` and is guarded by tests in `test/tests/Connection/Test__Connection__Config.res`.
- Hardcoded download fallback now has explicit coverage for both failure modes:
  - native download failure → WASM retry
  - native channel resolution failure → direct WASM retry

### Remaining Test Work

- Align tests with new step-2 `PickedConnection` rule:
  - Add/adjust tests so command-lookup success sets `PickedConnection` to bare command (`"agda"`/`"als"`), not resolved absolute path.
  - Replace prior "non-auto-update on command discovery" expectations.

- Existing flake controls still present:
  - `This.retries(2)` appears in connection-related test files.
  - `it_skip` / `describe_skip` appears in selected integration/external tests.

- Reduce nondeterminism in connection tests:
  - Prefer deterministic mock platforms over `Desktop.make()` for contract tests.
  - Avoid assertions tied to host environment specifics (real PATH contents, local agda version).
  - Replace retry-based flake masking (`This.retries(2)`) with deterministic fixtures where possible.

- Keep integration tests explicit:
  - Network-tolerant tests in `test/tests/Connection/Test__Connection__Download__Util.res` should remain integration-oriented and not used as contract gates.
  - Skipped external/network tests (`describe_skip` / `it_skip`) should be documented as non-blocking integration coverage.

### Testing Strategy Notes

- Prefer behavioral contract assertions over UI rendering details (labels, separators, visual order).
- Use mock counters/flags only when they represent guaranteed control-flow points; otherwise assert final observable behavior.
- Keep assertions focused on stable state transitions (`connection.paths`, `PickedConnection`) and chain outcomes.

### Existing Coverage to Keep (Hardcoded/WASM)

- `test/tests/Connection/Test__Connection__Download.res` already covers `alreadyDownloaded` behavior for `Hardcoded`, including:
  - native path detection (`hardcoded-als/als`)
  - WASM path detection (`hardcoded-als/als.wasm`)
  - native-over-WASM preference when both exist
- Keep these tests as baseline coverage for native/WASM discovery semantics while adding higher-level chain/fallback contract tests.
