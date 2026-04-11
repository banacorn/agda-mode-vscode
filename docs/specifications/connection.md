# Connection Spec

> This is a target-state specification. Normative terms **MUST**, **SHOULD**, and **MAY** are used as defined by RFC 2119.

## Candidate

A candidate identifies a way to locate an Agda-related executable or server.
A candidate **MUST** be one of:
- **Filepath** — absolute path to an executable
- **Command name** — a bare name resolved via `$PATH` at runtime
- **URI** — a `file://` URI; on desktop **MUST** normalize to fsPath, on web **MUST** preserve URI scheme

## `connection.paths`

`connection.paths` **MUST** be an ordered list of unique Candidates.

The default value **MUST** be `["agda", "als"]`, declared in `package.json`.

## Download Fallback

Downloaded binaries **MUST** be tried in order:
- Desktop: [native, WASM]
- Web: [WASM]

On download success, the downloaded Candidate **MUST** be added to `connection.paths`.
Automatic fallback downloads **MUST** be prepended (lowest priority).
Manual UI-triggered downloads **MUST** be appended (highest priority).

## Download Domain Model

Download code **MUST** keep these concepts distinct:
- **Channel** — a moving pointer or update policy used to discover a release
- **Release** — a concrete ALS release tag
- **DownloadArtifact** — a concrete downloadable artifact belonging to a release
- **Candidate** — a local runnable target derived from an artifact or provided by the user

The download flow **MUST** be:

```text
Channel -> Release -> DownloadArtifact -> Candidate
```

`Channel` **MUST NOT** be used as the identity of a downloaded artifact or downloaded Candidate.

## Channels

A channel selects how an ALS release is discovered.

The following channels **MUST** be supported:
- **Dev** — resolves to the concrete `dev` release
- **Latest** — resolves to the current latest stable release

`Hardcoded` **MAY** exist temporarily as a legacy source policy, but **MUST NOT** be modeled as a Release and **MUST NOT** be used as the long-term channel model.

Channel selection **MUST** be made via UI picker and **MUST** be stored in memento.

The selected channel **MUST** apply to both automatic fallback downloads and manual UI-triggered downloads.
After channel selection, UI **MUST** reopen and show platform-appropriate download options for the selected channel.

Channels **MAY** expose explicit update operations:
- **Update Dev** — resolve `Channel.Dev` again and download from the current `dev` release artifacts
- **Update Latest** — resolve `Channel.Latest` again and download from the current latest stable release artifacts

Downloads discovered through different channels **MAY** coexist in `connection.paths`.
Switching channels **MUST NOT** remove existing downloaded Candidates.

## Releases

A Release is a concrete ALS release tag.

Examples:
- `dev`
- `v6`
- `v7`

`v6` **MUST** be modeled as a Release, not a Channel.

`dev` **MAY** appear as both:
- `Channel.Dev` — the update/discovery policy
- `Release("dev")` — the concrete GitHub release tag resolved by that channel

`Channel.Latest` **MUST** resolve to a concrete Release before selecting or downloading artifacts.

## DownloadArtifact

A DownloadArtifact identifies one canonical ALS release artifact.

A DownloadArtifact **MUST** include:
- `releaseTag: string`
- `agdaVersion: string`
- `platform: Platform.t`

`releaseTag` **MUST** be a non-empty single name segment and **MUST NOT** contain `/` or `-`.

`releaseTag` **MAY** contain dots, for example `v6.1`.

`agdaVersion` **MUST** be a dot-separated numeric Agda version.

`Platform.t` **MUST** be a closed variant with exactly these canonical values:
- `Wasm` — asset tag `wasm`
- `Ubuntu` — asset tag `ubuntu`
- `MacOSArm64` — asset tag `macos-arm64`
- `MacOSX64` — asset tag `macos-x64`
- `Windows` — asset tag `windows`

DownloadArtifact parsing **MUST** accept only canonical ALS artifact filenames and artifact directory names:

```text
als-<releaseTag>-Agda-<agdaVersion>-<platform>.wasm
als-<releaseTag>-Agda-<agdaVersion>-<platform>.zip
als-<releaseTag>-Agda-<agdaVersion>-<platform>
```

The extension **MUST** be validated against the platform:
- `wasm` artifacts **MUST** use `.wasm` when an extension is present
- native artifacts **MUST** use `.zip` when an extension is present

Parsing **MUST** reject:
- wrong `Agda` casing
- unknown platforms
- mismatched extensions
- arbitrary full paths
- malformed names

Malformed names **MUST NOT** be repaired, migrated, displayed as downloaded, or shown as `version unknown`.

The artifact directory name **MUST** be exactly the canonical artifact name without extension:

```text
als-<releaseTag>-Agda-<agdaVersion>-<platform>
```

## Managed Download Storage

Managed downloads **MUST** be stored under a release namespace:

```text
<globalStorage>/releases/<releaseTag>/<artifactDirName>/als
<globalStorage>/releases/<releaseTag>/<artifactDirName>/als.wasm
```

The `<releaseTag>` directory **MUST** match the parsed `DownloadArtifact.releaseTag` from `<artifactDirName>`.

Examples:

```text
<globalStorage>/releases/dev/als-dev-Agda-2.8.0-wasm/als.wasm
<globalStorage>/releases/v6/als-v6-Agda-2.8.0-wasm/als.wasm
<globalStorage>/releases/v6/als-v6-Agda-2.8.0-macos-arm64/als
```

Downloaded Candidate discovery **MUST** scan managed releases globally:

```text
<globalStorage>/releases/*
```

Downloaded Candidate discovery **MUST NOT** be limited to the currently selected channel.

Top-level entries outside `<globalStorage>/releases/` **MUST NOT** be treated as managed downloaded Candidates.

Malformed entries under `<globalStorage>/releases/` **MUST** be ignored.

## `PreferredCandidate`

`PreferredCandidate` **MUST** be stored outside of `connection.paths`.

`PreferredCandidate` state transitions:
- `None → Some` — allowed only by explicit user action
- `Some → Some` — allowed only by explicit user action
- `Some → None` — **forbidden**

Explicit user actions that set `PreferredCandidate`:
- user candidate selection in Picker UI

Successful manual UI-triggered download **MUST NOT** modify `PreferredCandidate`.

## Delete Downloads

"Delete Downloads" **MUST** remove all downloaded ALS binaries from disk, regardless of how the download was triggered (automatic fallback or manual UI-triggered).

"Delete Downloads" **MUST** remove the managed download storage root:

```text
<globalStorage>/releases/
```

"Delete Downloads" **MUST** remove download-managed paths from `connection.paths`.

"Delete Downloads" **MUST NOT** modify `PreferredCandidate`.

## Picker UI

The Picker UI **MUST** show the following sections:
- **Candidates** — shows one row for each Candidate in `connection.paths`. The row identity comes from the Candidate itself; label/detail/status **MAY** incorporate metadata from the most recent successful resolution/probe of that Candidate.
- **Download** — shows platform-appropriate download options for the selected channel, along with "Select other channel" and "Delete Downloads" options
  - A download option **MUST NOT** be shown if its Candidate is already in `connection.paths`
  - "Select other channel" **MUST** always be shown
  - "Delete Downloads" **MUST** always be shown

Candidates **MUST** be shown in reverse `connection.paths` order (highest priority first).

Each Candidate item **MUST** be displayed as:
- **label**: the version string of the executable; if the version is unknown, a fallback label **MUST** be shown
  - Agda → `"Agda 2.8.0"`
  - ALS → `"Agda 2.8.0 Language Server v6"` or `"$(squirrel)  Agda 2.8.0 Language Server v6"`
  - WASM ALS → `"Agda 2.8.0 Language Server v6 WASM"` or `"$(squirrel)  Agda 2.8.0 Language Server v6 WASM"`
  - Agda rows **MAY** show the existing Agda bird icon in addition to the label text
  - ALS rows **MAY** use the existing `$(squirrel)` codicon prefix
- **description**: `"selected"` if the Candidate is currently in use, empty otherwise
- **detail**: depends on Candidate type:
  - Filepath → the filepath
  - Command → `<command> (<resolved filepath>)`
  - URI → the URI string

Selecting a Candidate **MUST** set it as `PreferredCandidate`.

If `connection.paths` is empty, the Candidates section **MUST NOT** be shown.

Managed downloaded ALS Candidates **MUST** display the DownloadArtifact release tag and Agda version before connecting to the server.

Managed downloaded ALS Candidates **MUST NOT** use the unknown-version fallback label when their DownloadArtifact metadata is parseable.

After connecting, runtime probe metadata **MAY** refine the displayed ALS version.

## Resolution

Resolution **MUST** proceed in this order:

1. `PreferredCandidate` (if set)
2. `connection.paths` in reverse order
3. Download fallback

Each step **MUST** be attempted only after all prior steps fail.
