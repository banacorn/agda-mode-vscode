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
- `PickedConnection` MUST NOT be auto-cleared or demoted on connection failure.
- `PickedConnection` MUST only be changed by an explicit user action (Switch Version UI) or a download action. It MUST NOT be changed by automatic command lookup during connection attempts (step 2).
- When a download completes (chain or UI-triggered), `PickedConnection` MUST be set to the downloaded path.
- When the user selects an endpoint in the Switch Version UI, `PickedConnection` MUST be updated. The selected path MUST NOT be added to `connection.paths`.

**Switch Version UI**
- The channel selection UI MUST be hidden when fewer than two channels are available.

## The Connection Chain

The chain is tried in order, stopping at the first success. Failure at any step is non-fatal — the chain continues to the next step.

0. **Picked** — `PickedConnection` if set, regardless of whether it appears in `connection.paths`
1. **Paths** — remaining entries in `agdaMode.connection.paths` (excluding any entry that exactly matches `PickedConnection`)
2. **Commands** — `agda`, `als` looked up from PATH (skipped individually if the exact string `"agda"` or `"als"` appears in `connection.paths` or equals `PickedConnection`)
3. **Download native** — download native binary via the active channel
4. **Download WASM** — fallback if native download fails

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
- `PickedConnection` changes only via explicit user action (Switch Version UI) or a download action that sets it.

`PickedConnection` is set in three ways:

1. **Chain auto-download** — when the chain downloads ALS as a fallback (steps 3–4), it sets both `connection.paths` and `PickedConnection`
2. **UI download** — when the user triggers a download from the Switch Version UI, same result
3. **UI endpoint selection** — when the user selects an entry (from `connection.paths` or PATH-discovered), only `PickedConnection` is updated. The path is never added to `connection.paths` here.

## Switch Version UI

The Switch Version UI has three jobs:

1. **Endpoint selection** — lists all entries in `agdaMode.connection.paths` plus PATH-discovered endpoints (`agda`, `als`). Selecting one sets `PickedConnection` only — nothing is added to `connection.paths`.

2. **Download** — allows the user to download ALS (native) or WASM. A successful download adds the path to `agdaMode.connection.paths` and sets `PickedConnection` to it.

3. **Channel selection** — lists available channels and allows the user to change the active channel. Hidden when only one channel is available (currently the case — only `Hardcoded` exists).

## Implementation Issues

- **[High] `PickedConnection` guard in chain** — `Connection.res:526` only prioritizes `PickedConnection` if the path is in `connection.paths`. Must be removed so `PickedConnection` is respected unconditionally (step 0).

- **[High] `PickedConnection` is auto-updated on normal connection success** — `Connection.res:550` sets `PickedConnection` to `getPath(connection)` for any successful connection (including step 2 automatic command lookup). Spec forbids automatic command-lookup updates and only allows explicit UI actions or download actions to change `PickedConnection`.

- **[High] UI endpoint selection adds to `connection.paths`** — `State__SwitchVersion.res:446` calls `addAgdaPath` when the user selects an endpoint. Spec says selection only updates `PickedConnection`.

- **[Medium] UI download does not set `PickedConnection`** — `State__SwitchVersion.res` lines 640, 654, 703 call `addAgdaPath` but never set `PickedConnection`. Spec requires both.

- **[Medium] Command step does not apply skip filters** — `Connection.res:544` always appends `agda`/`als` command probes with no filtering. Spec says skip them when `"agda"`/`"als"` is present in `connection.paths` or equals `PickedConnection`.

- **[Medium] Bare-command UI selection is not preserved as selected** — `State__SwitchVersion.res:827` parses selected strings through `Connection.URI.parse`, and `switchAgdaVersion` stores `VSCode.Uri.fsPath` for file URIs (`State__SwitchVersion.res:425`, `:445`). This normalizes command-like selections to absolute paths instead of preserving the user-selected bare command where applicable.

- **[Medium] Tests enforce old endpoint-selection behavior** — `Test__Connection__Config.res:447` and `:487` expect the selected path to be appended to `connection.paths`, conflicting with the spec.

- **[Medium] Tests enforce old auto-update behavior for `PickedConnection`** — `Test__Connection__Memento.res:137` expects auto discovery (command lookup) to update `PickedConnection`, conflicting with the spec's "no automatic command-lookup update" rule.

- **[Low] Only `Hardcoded` channel is available** — `LatestALS` and `DevALS` are broken and not yet reintroduced. Channel selection UI is hidden until more than one channel is available.

## Testing to Add/Fix

This section tracks the current status of test alignment with this spec.

### Completed

- Download-failure tests are fixed and passing in `test/tests/Test__Connection.res`:
  - `fromDownloads` / `should throw the \`DownloadALS\` error when the download policy is \`Yes\` but the download fails`
  - `make fromDownloads scenarios` / `should handle download failure with logging`
- Spec-aligned UI/memento tests were updated:
  - `test/tests/Connection/Test__Connection__Config.res`
  - `test/tests/Connection/Test__Connection__Memento.res`
- Contract coverage was added in `test/tests/Test__Connection.res`:
  - `"should try PickedConnection first even when it is not in connection.paths"`
  - `"should continue to later steps when PickedConnection fails"`
  - `"should not re-probe PickedConnection in the paths step"`
  - `"should skip agda/als command probes when already present in connection.paths"`
  - `"should not persist resolved absolute command paths back into connection.paths"`
  - `"should update both connection.paths and PickedConnection after successful download"`
  - Current result: 3 pass (`not re-probe`, `not persist`, `download updates both`), 3 red (listed below) pending implementation changes.

### Currently Red (Expected Until Implementation Catches Up)

- `test/tests/Connection/Test__Connection__Config.res`
  - `"should update PickedConnection without modifying config when user selects existing path not in config"`
  - `"should update PickedConnection when user selects different path"`
  - `"should add downloaded path to config when user downloads new ALS"`
  - `"should add already downloaded path to config when user selects already downloaded ALS"`
- `test/tests/Connection/Test__Connection__Memento.res`
  - `"should not set memento to working connection path from auto discovery"`
- `test/tests/Test__Connection.res`
  - `"should try PickedConnection first even when it is not in connection.paths"`
  - `"should continue to later steps when PickedConnection fails"`
  - `"should skip agda/als command probes when already present in connection.paths"`

### Remaining Test Work

- Verified current flake controls in the repo (as of this review):
  - `This.retries(2)` appears in connection-related tests, including `test/tests/Test__Connection.res`, `test/tests/Connection/Test__Connection__Memento.res`, and `test/tests/Connection/Test__Connection__Config.res`.
  - `it_skip` / `describe_skip` appears in `test/tests/Test__Connection.res`, `test/tests/Connection/Test__Connection__Process.res`, and `test/tests/Connection/Test__Connection__Download.res`.

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
