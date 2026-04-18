# Connection Refactor Plan

This plan drains `State__SwitchVersion` until it can be deleted.

This document is transient. It describes module ownership and migration order only.
Behavioral/domain requirements belong in `connection.md`.

The target ownership is:

```text
Connection__Download*  = download domain
Connection__UI*        = connection picker UI
Connection.res         = public facade and connection orchestration
State__SwitchVersion   = temporary shell to be drained and hard-deleted
```

## Goal

`State__SwitchVersion` and `Test__State__SwitchVersion*` should disappear.

Code should move by responsibility, not by filename:

- Download-related behavior goes to `Connection__Download*`.
- Connection picker UI behavior goes to `Connection__UI*`.
- Remaining connection orchestration goes to `Connection.res`.

`Connection.res` should remain the public facade that other modules call.

Internal APIs may be broken during refactor slices as long as tests are updated in the same slice.

## Ownership Rules

### Download Domain

`Connection__Download*` owns download-domain behavior.

This includes:

- channels as canonical identities
- releases
- artifacts
- asset enumeration
- source selection
- availability construction
- managed storage discovery
- delete-download planning

`Connection__Download.Channel` should keep identity only:

```rescript
toString
fromString
all
```

Presentation details such as labels, descriptions, and picker items should not live in `Connection__Download.Channel` long term.

### Picker UI

`Connection__UI*` owns only the connection picker UI.

This includes:

- picker item data
- row labels and descriptions
- section rendering
- picker view wrapper
- picker handlers
- channel submenu rendering

`Connection__UI*` may call VS Code picker APIs directly.
Tests should use seams at handler boundaries.
Most UI tests should be pure item-data tests; keep only narrow QuickPick integration tests where runtime VS Code behavior matters.

Presentation helpers should move to:

```text
Connection__UI__Labels
```

### Orchestration

`Connection.res` owns connection orchestration.

This includes:

- activation-level connection behavior
- background availability update orchestration
- applying download delete plans
- coordinating domain and UI modules

If `Connection.res` becomes too large, introduce a focused helper:

```text
Connection__Switch
```

`Connection__UI*` should render background update states, but should not own background update orchestration.

## Download Module Split

### Remote Flow

`Connection__Download__Flow` owns remote download flow:

```text
Channel -> Release -> DownloadArtifact -> Source
```

It should choose a `Source` for a selected download option.

### Managed Storage

`Connection__Download__ManagedStorage` owns local managed storage:

```text
globalStorage -> DownloadArtifact -> Candidate
```

It should discover existing managed Candidates and map local managed artifacts back to Candidates.

The already-downloaded click path should move to `Connection__Download__ManagedStorage`, not `Connection__Download__Flow`.

### Delete Downloads

`Connection__Download__Delete` should eventually return a delete plan/result instead of applying all mutations directly.

Target shape:

```rescript
type result = {
  cleanedDirectories: array<VSCode.Uri.t>,
  failedUris: array<VSCode.Uri.t>,
  pathsToRemove: array<string>,
  metadataDirectoriesToClear: array<VSCode.Uri.t>,
  releaseCachesToClear: array<(string, string)>,
  inFlightFilesToDelete: array<VSCode.Uri.t>,
}
```

Applying mutations to config and memento should live in `Connection.res` or connection orchestration.

Direct mutation inside `Connection__Download__Delete.run` is acceptable only as an intermediate state.

### Availability

`Connection__Download__Availability` should eventually return typed domain records, not UI-shaped tuples.

Current intermediate shape:

```rescript
array<(bool, string, string)>
```

Target shape should be a typed record carrying domain data. Exact fields may evolve, but it should not be a UI tuple.

Rendering availability into picker rows belongs in `Connection__UI*`.

### Platform And Variant Modeling

Prefer concrete platform variants over broad UI variants:

```rescript
Ubuntu | MacOSArm64 | MacOSX64 | Windows | Wasm
```

Avoid `Native | WASM` unless there is a concrete UI reason to keep that grouping.

If the UI needs grouping, derive it from platform:

```rescript
isWasm(platform)
isNative(platform)
```

## Migration Order

### 1. Drain Download Domain

Move all download-domain behavior out of `State__SwitchVersion`.

This includes:

- existing downloaded Candidate lookup
- managed storage scanning
- artifact parsing
- channel resolution for downloads
- source selection
- download availability construction
- delete-download cleanup semantics

Target modules:

```text
Connection__Download__Availability
Connection__Download__Assets
Connection__Download__Delete
Connection__Download__Flow
Connection__Download__ManagedStorage
```

End state:

`State__SwitchVersion` does not parse artifacts, scan managed storage, resolve download channels, choose assets, choose sources, or delete managed downloads.

### 2. Extract Picker Data Model

Move QuickPick row data construction out of `State__SwitchVersion`.

This includes:

- item data variants
- separators
- candidate row rendering data
- download row rendering data
- labels and descriptions
- selected-row display state

Target modules:

```text
Connection__UI__ItemData
Connection__UI__Item
Connection__UI__Labels
```

End state:

`State__SwitchVersion` does not know how to construct picker rows.

### 3. Extract Picker View And Handlers

Move VS Code picker behavior out of `State__SwitchVersion`.

This includes:

- picker view wrapper
- `onSelection`
- `onHide`
- channel submenu behavior
- picker item update flow

Target modules:

```text
Connection__UI__Picker
Connection__UI__Handlers
```

End state:

`State__SwitchVersion` does not own QuickPick behavior.

### 4. Move Orchestration Into Connection

Move activation and switch-connection orchestration into connection-owned code.

Preferred target:

```text
Connection.res
```

If `Connection.res` becomes too large, introduce:

```text
Connection__Switch
```

End state:

`State__SwitchVersion` is deleted.

### 5. Delete State Tests By Ownership

Move tests according to the same ownership rules.

Download tests go to:

```text
test/tests/Connection/Test__Connection__Download.res
test/tests/Test__Connection__Downloads.res
```

Picker UI tests go to:

```text
test/tests/Connection/Test__Connection__UI.res
```

Connection orchestration tests go to:

```text
test/tests/Test__Connection.res
```

End state:

```text
no Test__State__SwitchVersion*
```

## Immediate Next Step

Move the already-downloaded selection path out of `State__SwitchVersion.Handler.handleDownload`.

Current smell:

- when `downloaded == true`, State scans `<globalStorage>/releases/*`
- State parses `DownloadArtifact`
- State filters by channel, variant, and version string
- State stats the executable and derives the Candidate path

That is managed-storage download-domain behavior.

### Red

Add connection tests for a helper such as:

```rescript
Connection__Download__ManagedStorage.findCandidateForSelection(
  globalStorageUri,
  ~channel,
  ~platform,
  ~versionString,
)
```

Test cases:

- DevALS native downloaded selection returns `<globalStorage>/releases/dev/als-dev-Agda-2.8.0-macos-arm64/als`.
- DevALS WASM downloaded selection returns the managed WASM Candidate path.
- LatestALS native downloaded selection returns `<globalStorage>/releases/v6/als-v6-Agda-2.8.0-macos-arm64/als`.
- malformed artifact directories are ignored.
- matching artifact directories with missing executables are ignored.
- non-matching `versionString` returns `None`.

### Green

Move the filesystem scan from `State__SwitchVersion.Handler.handleDownload` into the managed-storage helper.

`State__SwitchVersion` should only:

- call the helper
- add the returned Candidate to `connection.paths` when present
- show the existing UI toast
- emit existing UI events

### Refactor

Keep local managed-storage discovery in:

```text
Connection__Download__ManagedStorage
```
