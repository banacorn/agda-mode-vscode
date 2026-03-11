# Connection System Specification

## Purpose and Scope

This spec defines the normative behavior of the connection system, covering:
- **Chain ordering** — the sequence and conditions under which connection candidates are tried
- **State updates** — how `connection.paths` and `PickedConnection` are read and written
- **UI effects** — what the Switch Version UI exposes and what actions it performs

This spec does not cover:
- Download protocols or network implementation details
- Platform detection internals (how desktop vs web is determined)
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
- Endpoint selection in the Switch Version UI MUST NOT add the selected path to `connection.paths`.

**`PickedConnection` updates**
- `PickedConnection` MAY be an absolute path or a bare command name.
- `PickedConnection` MUST only be changed by:
  - step 2 success — set to the bare command name (e.g. `"agda"`), only if currently `None`
  - a download completing — set to the downloaded path
  - explicit endpoint selection in the Switch Version UI — set to the selected path/command
  - Delete Downloads action — cleared to `None`, but only if currently pointing to a downloaded path
- `PickedConnection` MUST NOT be auto-cleared or demoted on failure.

**Switch Version UI**
- The channel selection UI MUST be hidden when fewer than two channels are available.

## The Connection Chain

Step names and order only. Formal stop/continue behavior and skip rules are defined in **Normative Rules**.

0. **Picked** — `PickedConnection`
1. **Paths** — entries from `agdaMode.connection.paths` (after step-1 exclusion rule)
2. **Commands** — PATH `agda`, `als` (after step-2 skip rules)
3. **Download native** — download native binary via the active channel
4. **Download WASM** — fallback from step 3 when native fails

## Channels

A channel determines which version of ALS to download. The format fallback (native → WASM) is orthogonal to channel selection: regardless of which channel is active, native is always tried before WASM.

- `Hardcoded` — pinned known-good version
- `LatestALS` — latest release from GitHub
- `DevALS` — dev release


## `connection.paths`

`agdaMode.connection.paths` is a list of paths or command names tried at step 1 of the chain. It is managed primarily by the user, but the system also appends to it when a download completes. Entries can be absolute paths (e.g. `/usr/local/bin/agda`) or bare command names (e.g. `agda`) that are resolved from PATH on each run.

The list is probed in **reverse order** — the last entry is tried first. This is intentional and documented in the config schema (`package.json`: "from the LAST to the first").

By rule, successful downloads append (deduplicated) to `connection.paths`. Since `getAgdaPaths()` reverses before probing, appended downloads naturally have highest priority even without `PickedConnection` (see `package.json` config schema).

By rule, command-discovered paths (step 2) are never added to `connection.paths`. This preserves bare-command intent (`agda` should re-resolve from PATH each run; see PR #272).

## `PickedConnection`

`PickedConnection` (stored in memento) records the user's preferred connection path (see step 0 of the chain).

**Lifecycle rules:**
- `PickedConnection` is sticky and must not be auto-cleared or demoted on connection failure.
- Normative update rules are defined in **Normative Rules**.

`PickedConnection` is changed in these ways:

**Set:**
1. **Command lookup (step 2)** — when `agda` or `als` is found in PATH and used to connect, `PickedConnection` is set to the bare command name (e.g. `"agda"`), not the resolved absolute path. This only happens when `PickedConnection` is currently `None`; an existing pick is never overwritten by step 2.
2. **Chain auto-download** — when the chain downloads ALS as a fallback (steps 3–4), `PickedConnection` is set to the downloaded path.
3. **UI download variant selection** — when the user selects a download variant in the Switch Version UI (section 2), successful download sets `PickedConnection` to the downloaded path.
4. **UI endpoint selection** — when the user selects an entry (from `connection.paths` or PATH-discovered), `PickedConnection` is set to that selected path/command.

**Cleared:**
5. **Delete Downloads** — clears `PickedConnection` to `None`, but only if it currently points to a downloaded path. Non-download picks (system installs, bare commands) are left unchanged.

## Switch Version UI

The UI is a VSCode QuickPick with up to three sections ("Installed", "Download", "Misc"), always in this order, separated by VSCode QuickPick separators. The Installed section is hidden entirely (no separator, no placeholder) when no endpoints are found. When two or more channels are available, a standalone `"📡 Select other channels"` button is inserted between the Download section and the Misc section (it is not a section separator — just a button item).

### 1. Installed

Lists all available connection endpoints in a single merged list, in this fixed order:
- Entries from `connection.paths`, in reverse config order (highest priority first)
- PATH-discovered `agda` (if found in PATH), then `als` (if found in PATH)

Each endpoint item renders as (strings are normative; angle-bracket tokens like `<v>` are placeholders for runtime values):

| Endpoint type         | Label                          | Description                        | Detail | Icon              |
|-----------------------|--------------------------------|------------------------------------|--------|-------------------|
| `Agda(Some(v))`       | `"Agda <v>"`                   | `"Selected"` if marked, else `""`  | path   | dark/light `.png` |
| `Agda(None)`          | `"Agda (version unknown)"`     | `"Selected"` if marked, else `""`  | path   | dark/light `.png` |
| `ALS(Some(v, a, _))`  | `"ALS 🐿 <v>, Agda <a>"`       | `"Selected"` if marked, else `""`  | path   | —                 |
| `ALS(None)`           | `"ALS 🐿 (version unknown)"`   | `"Selected"` if marked, else `""`  | path   | —                 |
| `Unknown` + error     | `"$(error) <filename>"`        | `"Error: <error>"`                 | path   | —                 |
| `Unknown` (no error)  | `"$(question) <filename>"`     | `"Unknown executable"`             | path   | —                 |

Selection marking is determined by the following rules, in priority order:

| `PickedConnection`  | `state.connection`  | Marked entry                          |
|---------------------|---------------------|---------------------------------------|
| `Some(path)`        | any                 | entry whose path/command equals `path`|
| `None`              | `Some(conn)`        | entry matching `Connection.getPath(conn)` (fresh-install inference) |
| `None`              | `None`              | (nothing marked)                      |

If no endpoints exist (empty `connection.paths` and no PATH commands found), the entire Installed section is hidden — no separator and no placeholder item.

Endpoint selection behavior follows **Normative Rules**.

### 2. Download

Lists the download variants for the currently selected channel:

| Platform | Entries shown                     |
|----------|-----------------------------------|
| Desktop  | Native ALS entry + WASM ALS entry |
| Web      | WASM ALS entry only               |

Each entry shows the version string as detail. A variant is **hidden** from the Download section if and only if its expected download path (under the extension's known download directory) is already present in `connection.paths`. Such a path appears instead in the Installed section, preventing duplication. User-provided paths (manual installs, custom builds) are never in the download directory, so they never suppress a Download entry. Selecting an entry triggers an immediate download for that variant; state updates follow **Normative Rules**.

**Placeholder loading phase:** While channel metadata is being fetched asynchronously, the Download section shows a single disabled-looking item labelled `"Checking availability..."` in place of the real variants. Once metadata is ready, this placeholder is replaced with the actual variant entries. The Installed section is unaffected by this phase. Selection is allowed throughout — if the user picks a Download item while loading, the action proceeds normally once the item resolves.

### 3. Channels (hidden when fewer than 2 channels are available)

A single standalone button item labelled `"📡 Select other channels"`, placed between the Download section and the Misc section. It is not a section with a header — just a button in the item list. The currently active channel is visible in the Download section header (e.g. `"Download (channel: Hardcoded)"`), so the button label does not repeat it.

Selecting the button opens a **sub-QuickPick** listing all available channels (`Hardcoded`, `LatestALS`, `DevALS` — DevALS only when DevMode is enabled). After the user picks a channel, the main QuickPick stays open and the Download section refreshes to show that channel's variants.

### 4. Misc

- **Delete Downloads** (`"$(trash)  Delete downloads"`) — clears ALL downloaded ALS from global storage (every channel).

### Actions

| Action                  | Guard                | Effect on `PickedConnection`  | Effect on `connection.paths`       | Other effects                                      |
|-------------------------|----------------------|-------------------------------|------------------------------------|----------------------------------------------------|
| Select endpoint         | —                    | set to selected path/command  | unchanged                          | initiates connection attempt                       |
| Select download variant | variant shown for current platform (see Download section table) | set to downloaded path        | downloaded path appended (deduped) | download triggered; UI refreshes                   |
| Select channel          | ≥ 2 channels         | unchanged                     | unchanged                          | Download section refreshes with new channel variants|
| Delete Downloads        | —                    | cleared (`None`) if pointing to a downloaded path, else unchanged | downloaded paths removed           | `Memento.Endpoints` cleared; download dirs deleted |

### Download section (absent as a top-level command)

There is no standalone "Download" command. Downloads are triggered by selecting a download variant in the Switch Version UI (section 2 above) or automatically via the connection chain (steps 3–4).

## Implementation Issues

Coverage tags: `PARTIAL` / `CONTRADICTORY` / `NONE`
Stable IDs: numbering is an identifier and is not renumbered after removals.

1. **[Coverage: PARTIAL] Step-2 command success overwrites existing `PickedConnection`.**
   Spec says step 2 sets `PickedConnection` only when it is currently `None`.
   Implementation currently sets it unconditionally on step-2 success.
   Covered for the `None` case (`Test__Connection__Memento`), but no test guards the "must not overwrite existing pick" case.

2. **[Coverage: PARTIAL] Delete Downloads unconditionally clears `PickedConnection`.**
   Spec says `PickedConnection` should only be cleared if it points to a downloaded path.
   Implementation calls `Memento.PickedConnection.clear` unconditionally (`State__SwitchVersion.res:776`).
   Covered for downloaded picks (`Test__State__SwitchVersion`), but no test guards "non-download picks remain unchanged."

3. **[Coverage: CONTRADICTORY] Download section shows per-channel items, not per-variant items.**
   Spec says the Download section lists native + WASM variants for the currently selected channel.
   Implementation shows one download item per channel (`LatestALS`, `DevALS`), not per variant.
   Current item-data tests assert existing `DownloadAction(..., "latest"/"dev")` behavior.

4. **[Coverage: NONE] Channels section not implemented as sub-QuickPick.**
   Spec says section 3 shows a single button that opens a sub-QuickPick to select a channel, leaving the main QuickPick open.
   Implementation does not have this sub-QuickPick flow.

5. **[Coverage: NONE] Channels section not implemented as sub-QuickPick.**
   Spec says section 3 shows a single button that opens a sub-QuickPick to select a channel, leaving the main QuickPick open.
   Implementation does not have this sub-QuickPick flow.

6. **[Coverage: PARTIAL] Historical ID — managed download variants are suppressed when already present in `connection.paths`.**
   Spec says a download variant is hidden iff its expected download path is already in `connection.paths`.
   Implementation now filters managed variants before rendering the Download section.
   Guarded by passing test: `"should hide native download variant when its managed path is already in connection.paths"` (`Test__State__SwitchVersion`).
   Remaining coverage gap: no dedicated WASM suppression assertion yet.

7. **[Coverage: PARTIAL] Historical ID — placeholder label no longer falls through to endpoint selection.**
   Spec says selection is allowed throughout the placeholder phase, and the action proceeds normally once metadata resolves.
   Implementation now handles `"Checking availability..."` explicitly and avoids endpoint fallback.
   Guarded by passing test: `"should not treat checking-availability placeholder as endpoint selection"` (`Test__State__SwitchVersion`).
   Remaining coverage gap: no test yet asserts deferred download execution for a placeholder-originated click.

8. **[Coverage: PARTIAL] Download section header does not show active channel.**
   Spec says the active channel is visible in the Download section header.
   Implementation uses a fixed `"Download"` separator without channel state.
   Guarded by failing test: `"should include active channel in download section header"` (`Test__State__SwitchVersion`).

9. **[Coverage: PARTIAL] Historical ID — Installed section is hidden when no endpoints are found.**
   Spec requires no Installed separator and no placeholder when endpoint list is empty.
   Implementation now matches this behavior.
   Guarded by passing test: `"should hide Installed section when no endpoints are found"` (`Test__State__SwitchVersion`).

10. **[Coverage: NONE] Selection handler treats unknown items as endpoints.**
   Any item that doesn't match `deleteDownloads`, `downloadNativeALS`, or `downloadWasmALS` falls through to endpoint selection logic. A separator or unexpected item would be incorrectly processed. Low risk in practice since VSCode prevents selecting separators.

11. **[Coverage: NONE] Path format contract (fsPath vs URI) unspecified.**
   The implementation's `isPathUnderDownloadDirectory()` handles both filesystem path and URI string formats. The spec does not define which format paths in `connection.paths` are expected to be in.

## Testing to Add/Fix

This section lists test coverage expectations and remaining gaps for this spec.

### Contract Coverage

- Connection contract coverage in `test/tests/Test__Connection.res` should include:
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
- Switch Version cleanup coverage should include:
  - `"should hide Installed section when no endpoints are found"` in `test/tests/Test__State__SwitchVersion.res`
  - `"should remove download-managed paths from connection.paths on Delete Downloads"` in `test/tests/Test__State__SwitchVersion.res`
  - `"should not show native download option on web platform"` in `test/tests/Test__State__SwitchVersion.res`
  - `"should keep main quickpick open when selecting channel-switch button"` in `test/tests/Test__State__SwitchVersion.res`
  - `"should hide native download variant when its managed path is already in connection.paths"` in `test/tests/Test__State__SwitchVersion.res`
  - `"should not treat checking-availability placeholder as endpoint selection"` in `test/tests/Test__State__SwitchVersion.res`
  - `"should include active channel in download section header"` in `test/tests/Test__State__SwitchVersion.res`

### Remaining Test Work

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

## Future Work: Testable State Transitions

Currently `Handler.onSelection` mixes three concerns — deciding the new state, persisting it, and triggering side effects — making state transition logic untestable without mocking VSCode APIs.

### Proposed Refactor

Separate state decision from execution by introducing:

**1. An action type**
```rescript
type action =
  | SelectEndpoint(string)
  | SelectDownload(downloadVariant)
  | SelectChannel(channel)
  | DeleteDownloads
```

**2. A pure transition function**
```rescript
type stateUpdate = {
  pickedConnection: option<string>,
  pathsToAdd: array<string>,
  pathsToRemove: array<string>,
  activeChannel: channel,
}

let transition: (state, action) => stateUpdate
```

`transition` is pure — no I/O, no memento, no VSCode calls. It returns a description of what should change.

**3. A thin executor**
```rescript
let applyUpdate: (stateUpdate, context) => promise<unit>
```

Applies the update to memento, config, triggers downloads, etc. Intentionally left as integration-level only.

### Testing

Tests call `transition` directly and assert exact equality on the returned `stateUpdate`:

```rescript
Assert.deepStrictEqual(
  transition(initialState, SelectDownload(Native)),
  { pickedConnection: Some(downloadedPath), pathsToAdd: [downloadedPath], ... }
)
```

No mocking, no async, no VSCode required. The async render phases (filesystem sync, version probing) are irrelevant here — `transition` operates on already-resolved state at the point of user selection.

### Scope

The refactor surface is small: extract decision logic out of `Handler.onSelection` into `transition`, leave I/O wiring in place calling `applyUpdate`. The pure render pipeline (`ItemData.entriesToItemData` etc.) is already separately testable and does not need to change.
