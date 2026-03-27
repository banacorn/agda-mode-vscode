# Connection Spec (v2)

> This is a target-state specification. Normative terms **MUST**, **SHOULD**, and **MAY** are used as defined by RFC 2119.

## Candidate

A candidate identifies a way to locate ALS.
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

## Channels

A channel selects which ALS version is downloaded.

The following channels **MUST** be supported:
- Hardcoded (pinned known-good version)
- Dev/nightly (development pre-release builds)

Hardcoded **MUST** be the default channel.
> **Note:** Only Hardcoded is currently operational; Dev/nightly is planned.

Channel selection **MUST** be made via UI picker and **MUST** be stored in memento.

The selected channel **MUST** apply to both automatic fallback downloads and manual UI-triggered downloads.
After channel selection, UI **MUST** reopen and show platform-appropriate download options for the selected channel.

Downloads from different channels **MAY** coexist in `connection.paths`.
Switching channels **MUST NOT** remove existing downloaded Candidates.

## `PreferredCandidate`

`PreferredCandidate` **MUST** be stored outside of `connection.paths`.

`PreferredCandidate` state transitions:
- `None → Some` — allowed only by explicit user action
- `Some → Some` — allowed only by explicit user action
- `Some → None` — **forbidden**

Explicit user actions that set `PreferredCandidate`:
- user endpoint selection in Picker UI
- successful manual UI-triggered download

## Delete Downloads

"Delete Downloads" **MUST** remove all downloaded ALS binaries from disk, regardless of how the download was triggered (automatic fallback or manual UI-triggered).

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
  - WASM Agda → `"Agda 2.8.0 WASM"`
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

## Resolution

Resolution **MUST** proceed in this order:

1. `PreferredCandidate` (if set)
2. `connection.paths` in reverse order
3. Download fallback

Each step **MUST** be attempted only after all prior steps fail.
