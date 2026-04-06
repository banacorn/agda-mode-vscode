# Connection Spec

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
- user candidate selection in Picker UI

Successful manual UI-triggered download **MUST NOT** modify `PreferredCandidate`.

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

## TODO — Fix Download System Brittleness

The download system has several latent failure modes. All fixes use TDD (failing test first, then implementation).

### Implementation Order

- [ ] **Fix 1** — `suppressManagedVariants` async + `FS.stat`
- [ ] **Fix 3** — in-flight marker before cache check (+ new error variant)
- [ ] **Fix 4** — post-unzip binary verification (+ new error variant)
- [ ] **Fix 5** — `Unzip.run` resolves only after extraction output is finished
- [ ] **Fix 2** — startup in-flight cleanup (+ wiring test)
- [ ] **Fix 6** — background update fallback to Unavailable

Fixes 3, 4, and 5 are in the same GitHub download critical path. Fix 2 lands after core in-flight semantics are correct.

---

### Fix 1: `suppressManagedVariants` — verify artifact exists before suppressing

**Problem:** If `expectedPath` is in config but the file was deleted, the download item is filtered out. User sees no download button to recover.

**File:** `src/State/State__SwitchVersion.res` lines 578–594

**Change:** Make `suppressManagedVariants` async. Use `await FS.stat(uri)` on the reconstructed URI — not `NodeJs.Fs.existsSync(fsPath)`, because `globalStorageUri` may be `vscode-userdata:` or other non-`file:` forms. Suppress only when the candidate is in config **and** the artifact actually exists.

```rescript
let suppressManagedVariants = async (
  globalStorageUri: VSCode.Uri.t,
  configPaths: array<string>,
  downloadItems: array<(bool, string, string)>,
  ~channel: Connection__Download.Channel.t=Hardcoded,
): array<(bool, string, string)> => {
  let shouldKeep = async ((_, _, variantTag)) =>
    switch variantFromTag(variantTag) {
    | None => true
    | Some(variant) =>
      let uri = VSCode.Uri.joinPath(globalStorageUri, [channelToDirName(channel), variantFileName(variant)])
      let expectedPath = expectedPathForVariant(globalStorageUri, variant, ~channel)
      let expectedCandidate = Candidate.make(expectedPath)
      let inConfig = configPaths->Array.some(configPath =>
        Candidate.equal(Candidate.make(configPath), expectedCandidate))
      let fileExists = (await FS.stat(uri))->Result.isOk
      !(inConfig && fileExists)
    }
  let keeps = await Promise.all(downloadItems->Array.map(shouldKeep))
  downloadItems->Array.filterWithIndex((item, i) => keeps->Array.getExn(i))
}
```

Update **all** call sites — audit every caller (`getAllAvailableDownloads` at line 750, plus any tests or helpers that assume it is pure/sync).

**Test file:** `test/tests/Test__State__SwitchVersion__Downloads.res`

---

### Fix 2: In-flight cleanup on startup

**Problem:** If a download crashes, `in-flight.download` persists and permanently blocks future downloads with "Already downloading".

**Files:**
- `src/Connection/Download/Connection__Download__GitHub.res` — add and export `cleanupInFlightFiles`
- `src/Main/Main.res` — call it at the top of `activateWithoutContext`

```rescript
// New exported function
let cleanupInFlightFiles = async (globalStorageUri: VSCode.Uri.t): unit => {
  let inFlightUri = VSCode.Uri.joinPath(globalStorageUri, ["in-flight.download"])
  let inFlightZipUri = VSCode.Uri.joinPath(globalStorageUri, ["in-flight.download.zip"])
  let _ = await FS.delete(inFlightUri)
  let _ = await FS.delete(inFlightZipUri)
}

// In activateWithoutContext, after globalStorageUri is available:
// Fire-and-forget: startup cleanup failures are ignored by design.
// Do not change this to await — cleanup must never block activation.
let _ = GitHub.cleanupInFlightFiles(globalStorageUri)
```

**Tests (two levels required):**
1. **Helper test** in `test/tests/Connection/Test__Connection__Download.res` — create both in-flight files, call `GitHub.cleanupInFlightFiles`, assert both are gone.
2. **Wiring test** — prove `activateWithoutContext` actually calls the cleanup. Prefer a narrow seam (existing observable effect, log event, etc.) over full activation plumbing. Goal: removing the call site must cause a test failure.

---

### Fix 3: Create in-flight marker before cache check

**Problem:** There is a window between `isDownloading()=false` and `downloadLanguageServer` creating the in-flight file. A crash in that window leaves no sentinel.

**File:** `src/Connection/Download/Connection__Download__GitHub.res`, `Module.download` (lines 503–527)

**Change:** After `isDownloading()` returns false, immediately write the in-flight file. If `FS.writeFile` fails, return `Error` — do **not** silently continue (that makes the slot claim unreliable). `downloadLanguageServer` will overwrite it — harmless. Add `CannotWriteInFlightFile(string)` to `Error.t` — `FS.writeFile` returns `result<unit, string>`, so no coercion needed.

```rescript
} else {
  let inFlightDownloadUri = VSCode.Uri.joinPath(repo.globalStorageUri, [inFlightDownloadFileName])
  // Claim slot — must succeed or abort
  switch await FS.writeFile(inFlightDownloadUri, Uint8Array.make([])) {
  | Error(e) => Error(Error.CannotWriteInFlightFile(e))
  | Ok() =>
    let fileName = if downloadDescriptor.asset.name->String.includes("wasm") { "als.wasm" } else { "als" }
    let destFileUri = VSCode.Uri.joinPath(destUri, [fileName])
    switch await FS.stat(destFileUri) {
    | Ok(_) =>
      // Binary already cached — release sentinel. Failure here is not silent:
      // leaving the sentinel behind recreates the "Already downloading" blockage.
      switch await FS.delete(inFlightDownloadUri) {
      | Error(e) => Error(Error.CannotDeleteFile(e))
      | Ok() => Ok(true)
      }
    | Error(_) =>
      switch await downloadLanguageServer(repo, reportProgress, downloadDescriptor) { ... }
    }
  }
}
```

**Test file:** `test/tests/Connection/Test__Connection__Download.res`

---

### Fix 4: Post-unzip binary verification

**Problem:** `Unzip.run()` can silently succeed without extracting the binary. User gets a "successfully downloaded" toast but the binary is missing.

**Files:** `src/Connection/Download/Connection__Download__GitHub.res` (`downloadLanguageServer` ZIP branch, ~lines 460–474) and `Error.t`

**Changes:**
1. Add `BinaryMissingAfterExtraction(string)` to `Error.t` (with `toString` case)
2. After `Unzip.run()` returns `Ok()`, stat `destPath/als` — this intentionally hardcodes `als` because native ZIP downloads always produce `als`
3. If missing: return `Error(BinaryMissingAfterExtraction(path))`
4. If present: delete `in-flight.download.zip`, then return `Ok()`

Correct sequence:
```
Unzip.run(...)
  → Ok()
    → FS.stat(destPath/als)
      → Error(_)  →  BinaryMissingAfterExtraction
      → Ok(_)     →  FS.delete(inFlightDownloadZipUri)
                       → Ok()     →  Ok()
                       → Error(e) →  CannotDeleteFile(e)
```

```rescript
| Ok() =>
  let execUri = VSCode.Uri.joinPath(destPath, ["als"])
  let execPath = VSCode.Uri.fsPath(execUri)
  switch await FS.stat(execUri) {
  | Error(_) =>
    Error(Error.BinaryMissingAfterExtraction(execPath))
  | Ok(_) =>
    switch await FS.delete(inFlightDownloadZipUri) {
    | Error(e) => Error(Error.CannotDeleteFile(e))
    | Ok() => Ok()
    }
  }
```

**Test file:** `test/tests/Connection/Test__Connection__Download.res`

---

### Fix 5: `Unzip.run` must wait for extraction output completion

**Problem:** `Unzip.run()` currently resolves on the ZIP input read stream `close` event. That only proves the input stream closed; it does **not** prove `unzipper.Extract(...)` has finished writing extracted files. After Fix 4, native ZIP downloads immediately stat `destPath/als`, so they can falsely return `BinaryMissingAfterExtraction` while extraction is still writing.

**File:** `src/Connection/Download/Connection__Download__Unzip.res`

**Change:** Resolve `Unzip.run(...)` only when the extractor output stream has finished/closed. Handle stream errors from both input and extraction output.

Current broken shape:
```rescript
let readStream = NodeJs.Fs.createReadStream(srcUri->VSCode.Uri.fsPath)
readStream->NodeJs.Fs.ReadStream.onCloseOnce(resolve)->ignore

readStream
->NodeJs.Fs.ReadStream.pipe(Unzipper.extract({"path": destUri->VSCode.Uri.fsPath}))
->ignore
```

Required behavior:
- input stream error → reject/fail the unzip promise
- extraction stream error → reject/fail the unzip promise
- extraction stream finish/close → resolve the unzip promise

**Test file:** `test/tests/Connection/Test__Connection__Download.res`

---

### Fix 6: Background update error → show Unavailable

**Problem:** The `backgroundUpdate` catch block swallows all errors. If `getAllAvailableDownloads` fails (e.g. GitHub rate-limited), the UI freezes on "Checking availability..." forever.

**File:** `src/State/State__SwitchVersion.res` lines 1237–1239

**Change:** In the catch block, replace the placeholder items with `unavailableItem` fallbacks.

```rescript
} catch {
| _exn =>
  let fallback = switch await PlatformOps.determinePlatform() {
  | Ok(Connection__Download__Platform.Web) => [unavailableItem(WASM)]
  | _ => [unavailableItem(Native), unavailableItem(WASM)]
  }
  try { await updateUI(fallback) } catch { | _ => () }
}
```

**Test file:** `test/tests/Test__State__SwitchVersion__Downloads.res`
