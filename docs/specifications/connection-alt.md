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

`PreferredCandidate` state transitions:
- `None → Some` — allowed only by explicit user action
- `Some → Some` — allowed only by explicit user action
- `Some → None` — **forbidden**

Explicit user actions that set `PreferredCandidate`:
- user endpoint selection in Switch Version UI
- successful manual UI-triggered download

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

- [x] **T1** Delete Downloads — spec L62 says MUST NOT modify `connection.paths`/`PreferredCandidate`, but tests assert it removes download-managed paths and clears `PickedConnection` when pointing to a download path (`Test__State__SwitchVersion.res:1195,1244,1305`) — **tests fixed**, implementation still pending (see I1)
- [x] **T2** Resolution order — spec L69 says "reverse order", but tests and implementation use forward order (`Test__Connection__Config.res:124-134`, no `reverse` call in `Connection*.res`) — **tests fixed**, implementation still pending (see I2)
- [x] **T3** Automatic fallback setting PreferredCandidate — spec L52-54 says only explicit user action, but automatic fallback via `makeWithFallback` sets `PickedConnection` (`Test__Connection.res:2357-2386`) — **tests fixed**, implementation still pending (see I3)
- [x] **T4** Manual UI download setting PreferredCandidate — spec L54 lists it as a trigger, but test asserts `pickedConnection = None` after manual download (`Test__Connection__Config.res:576`)

### Missing Test Coverage

- [x] **T5** Add test: automatic fallback download prepends to `connection.paths` (spec L26, no test verifies position)
- [x] **T6** Add test: `connection.paths` rejects or deduplicates duplicate Candidates (spec L15)
- [x] **T7** Add test: channel selection persists in memento (spec L40)
- [x] **T8** Add test: switching channels preserves existing downloaded Candidates (spec L46)
- [ ] **T9** Add test: default value of `connection.paths` is `["agda", "als"]` (spec L17)
- [ ] **T10** Add test: download order is [native, WASM] on Desktop and [WASM] on Web (spec L21-23)
- [ ] **T11** Add test: Hardcoded is the default channel on fresh activation (spec L37)
- [x] **T12** ~~Add test: PreferredCandidate can be explicitly cleared by user action in Switch Version UI~~ — removed from spec; PreferredCandidate is never cleared, only overwritten

### Spec-vs-Implementation Contradictions

- [x] **I1** Delete Downloads — spec L62 says MUST NOT modify, but implementation removes download-managed paths from config (`State__SwitchVersion.res:753-762` `removeDownloadedPathsFromConfig`) and clears `PickedConnection` when pointing to a download path (`State__SwitchVersion.res:903-906`) — tests aligned with spec L62: preserve paths+picked (Test__State__SwitchVersion.res:1195), preserve picked when under download dir (Test__State__SwitchVersion.res:1881), preserve picked when not under download dir (Test__State__SwitchVersion.res:1958)
- [x] **I2** Resolution order — spec L69 says "reverse order", but `Config.res:79` `parseAgdaPaths` already reverses with `Array.toReversed`, then `Connection.res:583-588` iterates forward. Net effect: last entry in user config = highest priority. The "reverse" happens at parse time, not resolution time. — guarded by T2 tests (reverse order priority: Test__Connection__Config.res:121) and parse-time reverse test (Test__Config.res:55)
- [x] **I3** Automatic fallback sets PreferredCandidate — spec L52-54 says only explicit user action, but `Connection.res:637` calls `Memento.PickedConnection.set` after automatic fallback download — tests aligned with spec: no picked after fallback (Test__Connection.res:2506), existing picked preserved (Test__Connection.res:2537)
- [x] **I4** Manual UI download does NOT set PreferredCandidate — spec L54 lists it as a trigger, but `State__SwitchVersion.res:800-838` (`handleDownload`) only calls `addAgdaPath`, never sets `PickedConnection` — tests aligned with spec: picked set after new download (Test__Connection__Config.res:642), after already-downloaded (Test__Connection__Config.res:660), via handler (Test__State__SwitchVersion.res:1463), via onSelection (Test__State__SwitchVersion.res:1552)
- [x] **I5** Automatic fallback appends instead of prepends — spec L26 says "prepended (lowest priority)", but `Connection.res:636` calls `addAgdaPath` which appends (`Config.res:101` `Array.concat(paths, [path])`) — test aligned with spec: prepend assertion (Test__Connection.res:2570)
- [x] **I6** Uniqueness only enforced on add, not on bulk set — `Config.res:94-96` `addAgdaPath` checks `Array.includes` before adding, but `Config.res:116-129` `setAgdaPaths` and `Config.res:68-80` `parseAgdaPaths` do not deduplicate — tests aligned with spec: setAgdaPaths dedup assertion (Test__Connection__Config.res:359), parseAgdaPaths dedup assertion (Test__Config.res:78)
- [x] **I7** Channel selection NOT stored in memento — spec L40 says MUST be stored, but `State__SwitchVersion.res:1007` uses a local `ref(Hardcoded)` recreated on every UI activation; no `SelectedChannel` module exists in `Memento.res` — tests aligned with spec: persist on switch (Test__State__SwitchVersion.res:1755), default fallback on fresh activation (Test__State__SwitchVersion.res:1848), default fallback on invalid label (Test__State__SwitchVersion.res:1875), round-trip restore of valid channel (Test__State__SwitchVersion.res:1895)
- [x] **I8** Switching channels preserves downloads — **COMPLIANT**, channel switching (`State__SwitchVersion.res:872-881`) only updates UI, does not touch config paths — test aligned with spec: paths unchanged after switch (Test__State__SwitchVersion.res:1791)
- [x] **I9** Channel selection is not applied to actual downloads — spec says selected channel must drive both automatic and manual downloads (connection-alt.md:42), but automatic fallback hardcodes `Hardcoded` (`Connection.res:451`) and manual download source is also hardcoded (`State__SwitchVersion.res:588`) — tests aligned with spec: automatic fallback must use selected channel (Test__Connection.res:2602), manual download sourceForVariant must use selected channel (Test__State__SwitchVersion.res:2065), manual download end-to-end must use selected channel (Test__State__SwitchVersion.res:2105)
- [x] **I10** PreferredCandidate is still set by automatic command discovery — spec says only explicit user action may set it (connection-alt.md:52), but command-step success sets it when currently `None` (`Connection.res:625`) — tests aligned with spec: command discovery must not set PreferredCandidate when None (Test__Connection.res:2667), command discovery must not overwrite existing PreferredCandidate (Test__Connection.res:2705)
- [x] **I11** Resolution chain includes an extra command-probe phase not in the spec — spec order is exactly: preferred → connection.paths → download fallback (connection-alt.md:66), but impl adds a separate command phase between paths and downloads (`Connection.res:590`) — test aligned with spec: command probes must not be part of resolution chain (Test__Connection.res:2745)
- [x] **I12** Dev/nightly channel support is required by spec but not exposed by current UI availability — spec says Hardcoded + Dev/nightly must be supported (connection-alt.md:33), but available channels currently return only `[Hardcoded]` (`State__SwitchVersion.res:648`) — test aligned with spec: DevALS must be in available channels on Desktop (Test__State__SwitchVersion.res:2183)
- [x] **I13** `connection.paths` default still mismatches spec — spec requires `["agda", "als"]` (connection-alt.md:17), but impl default is `[]` in `package.json:1185` — test aligned with spec: default must be `["agda", "als"]` (Test__Connection__Config.res:295)
- [x] **I14** `connection.paths` persisted order can drift after writes — read path list is reversed at `Config.res:79`, but `addAgdaPath`/`setAgdaPaths` write the in-memory order back directly (`Config.res:101`, `Config.res:127`), which can flip existing persisted ordering across updates — tests aligned with spec: setAgdaPaths→getAgdaPaths round-trip must not reorder in production path (Test__Connection__Config.res:373), addAgdaPath must preserve existing order in production path (Test__Connection__Config.res:392)
- [x] **I15** Desktop URI normalization rule is violated for WASM candidates — spec says desktop `file://` candidates must normalize to fsPath (connection-alt.md:11), but impl explicitly preserves raw URI for `.wasm` at `Connection.res:160` — test aligned with spec: WASM probe must return fsPath, not raw URI (Test__Connection.res:2794)
- [x] **I16** Hardcoded-only runtime does not clamp restored channel — runtime channel availability is `[Hardcoded]` (`State__SwitchVersion.res:648`), but restored `selectedChannel` may remain DevALS/LatestALS from memento (`State__SwitchVersion.res:1021`), and UI header uses that value (`State__SwitchVersion.res:1033`) — test aligned with spec: onActivate download header must reference an available channel only (Test__State__SwitchVersion.res:2183)
- [x] **I17** Desktop native→WASM order does not cover native probe/connect failure path — WASM fallback is only in download/resolve failure branches (`Connection.res:467`, `Connection.res:499`); if native path is found (`alreadyDownloaded`) but `make(path, ...)` fails, code returns error without trying WASM (`Connection.res:485`, `Connection.res:519`) — test aligned with spec: WASM must be tried when cached native fails to connect (Test__Connection.res:2822)
- [x] **I18** Candidate type mismatch (spec narrower than implementation) — spec allows only Filepath, bare command, and `file://` URI (connection-alt.md:8), but implementation also supports `lsp://` and `vscode-*` URIs (`Connection__URI.res:37`, `Connection__URI.res:42`, `Connection.res:140`) — tests aligned with spec: lsp:// must be rejected (Test__Connection.res:2886), vscode-* must be rejected (Test__Connection.res:2901)
- [x] **I19** ~~PreferredCandidate explicit clear action~~ — removed from spec; PreferredCandidate is never cleared, only overwritten by explicit user action. Implementation and tests for the clear button should be removed.
- [x] **I20** Channel coexistence semantics are not modeled in download paths — spec allows downloads from different channels to coexist (connection-alt.md:45), but download path helpers are hardcoded to `hardcoded-als` and not channel-parameterized (`State__SwitchVersion.res:545`, `State__SwitchVersion.res:608`, `State__SwitchVersion.res:594`) — tests aligned with spec: isDownloaded false-positive for DevALS when only Hardcoded on disk (Test__State__SwitchVersion.res:2707), handleChannelSwitch Hardcoded→DevALS must change download item flags but both return identical [true,false] proving channel switch has no effect (Test__State__SwitchVersion.res:2754)
- [x] **I21** WASM path representation is inconsistent by download source — `FromURL` WASM returns URI string (`Connection__Download.res:143`, `Connection__Download.res:198`), but `FromGitHub` returns fsPath even for WASM (`Connection__Download.res:279`, `Connection__Download.res:288`); this conflicts with consistent candidate identity/normalization and can leak alias duplicates — test aligned with spec: real download(FromGitHub) cached branch returns fsPath, real downloadFromURL cached branch returns URI string for same WASM file — must be equal (Test__Connection.res:2921)
