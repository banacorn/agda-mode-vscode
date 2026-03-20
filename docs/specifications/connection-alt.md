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
After channel selection, UI **SHOULD** show platform-appropriate download options.

Downloads from different channels **MAY** coexist in `connection.paths`.
Switching channels **MUST NOT** remove existing downloaded Candidates.

## `PreferredCandidate`

`PreferredCandidate` **MUST** be stored outside of `connection.paths`.

`PreferredCandidate` **MUST** only be set by explicit user action:
- user endpoint selection in Switch Version UI
- successful manual UI-triggered download

`PreferredCandidate` **MUST** be cleared only by explicit user action in the Switch Version UI.

## Delete Downloads

"Delete Downloads" **MUST** remove all downloaded ALS binaries from disk, regardless of how the download was triggered (automatic fallback or manual UI-triggered).

"Delete Downloads" **MUST NOT** modify `connection.paths` or `PreferredCandidate`.

## Resolution

Resolution **MUST** proceed in this order:

1. `PreferredCandidate` (if set)
2. `connection.paths` in reverse order
3. Download fallback

Each step **MUST** be attempted only after all prior steps fail.

## TODO: Spec-vs-Test Gap Resolution

Decide for each item: fix the spec, fix the tests, or fix the implementation.

### Spec-vs-Test Contradictions

- [ ] **T1** Delete Downloads — spec L62 says MUST NOT modify `connection.paths`/`PreferredCandidate`, but tests assert it removes download-managed paths and clears `PickedConnection` when pointing to a download path (`Test__State__SwitchVersion.res:1195,1244,1305`)
- [ ] **T2** Resolution order — spec L69 says "reverse order", but tests and implementation use forward order (`Test__Connection__Config.res:124-134`, no `reverse` call in `Connection*.res`)
- [ ] **T3** Automatic fallback setting PreferredCandidate — spec L52-54 says only explicit user action, but automatic fallback via `makeWithFallback` sets `PickedConnection` (`Test__Connection.res:2357-2386`)
- [ ] **T4** Manual UI download setting PreferredCandidate — spec L54 lists it as a trigger, but test asserts `pickedConnection = None` after manual download (`Test__Connection__Config.res:576`)

### Missing Test Coverage

- [ ] **T5** Add test: automatic fallback download prepends to `connection.paths` (spec L26, no test verifies position)
- [ ] **T6** Add test: `connection.paths` rejects or deduplicates duplicate Candidates (spec L15)
- [ ] **T7** Add test: channel selection persists in memento (spec L40)
- [ ] **T8** Add test: switching channels preserves existing downloaded Candidates (spec L46)

### Spec-vs-Implementation Contradictions

- [ ] **I1** Delete Downloads — spec L62 says MUST NOT modify, but implementation removes download-managed paths from config (`State__SwitchVersion.res:753-762` `removeDownloadedPathsFromConfig`) and clears `PickedConnection` when pointing to a download path (`State__SwitchVersion.res:903-906`)
- [ ] **I2** Resolution order — spec L69 says "reverse order", but `Config.res:79` `parseAgdaPaths` already reverses with `Array.toReversed`, then `Connection.res:583-588` iterates forward. Net effect: last entry in user config = highest priority. The "reverse" happens at parse time, not resolution time.
- [ ] **I3** Automatic fallback sets PreferredCandidate — spec L52-54 says only explicit user action, but `Connection.res:637` calls `Memento.PickedConnection.set` after automatic fallback download
- [ ] **I4** Manual UI download does NOT set PreferredCandidate — spec L54 lists it as a trigger, but `State__SwitchVersion.res:800-838` (`handleDownload`) only calls `addAgdaPath`, never sets `PickedConnection`
- [ ] **I5** Automatic fallback appends instead of prepends — spec L26 says "prepended (lowest priority)", but `Connection.res:636` calls `addAgdaPath` which appends (`Config.res:101` `Array.concat(paths, [path])`)
- [ ] **I6** Uniqueness only enforced on add, not on bulk set — `Config.res:94-96` `addAgdaPath` checks `Array.includes` before adding, but `Config.res:116-129` `setAgdaPaths` and `Config.res:68-80` `parseAgdaPaths` do not deduplicate
- [ ] **I7** Channel selection NOT stored in memento — spec L40 says MUST be stored, but `State__SwitchVersion.res:1007` uses a local `ref(Hardcoded)` recreated on every UI activation; no `SelectedChannel` module exists in `Memento.res`
- [ ] **I8** Switching channels preserves downloads — **COMPLIANT**, channel switching (`State__SwitchVersion.res:872-881`) only updates UI, does not touch config paths
