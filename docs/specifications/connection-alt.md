# Connection Spec (v2)

> This is a **target-state specification**. Sections note what is currently operational vs. planned.

## `connection.candidates`

An ordered list of candidates. Each candidate is either a filepath, command name, or URI.

Default candidates: `agda`, `als`.

Candidates are tried in **reverse order** — the last candidate in the list has the highest priority.

## Download Fallback

A separate step triggered when **all** candidates in `connection.candidates` fail.

The **channel** determines which version of ALS to download. Three channels: Hardcoded, Latest release (default), Dev/nightly. The selected channel is stored in memento (extension state) and selected via UI picker.

- **Desktop**: try native binary first. If native fails, try WASM automatically.
- **Web**: try WASM only.

On success, the downloaded path is added to `connection.candidates`. **Automatic** fallback downloads are **prepended** (lowest priority). **Manual** (UI-triggered) downloads are **appended** (highest priority). Before adding, paths are normalized (resolve `file://` URIs, etc.) and compared — duplicates are not added.

Downloads from different channels coexist in `connection.candidates` — switching channels does not remove old downloads.

## Channels

An extension to the download feature. A channel selects which version of ALS to download.

- **Hardcoded** — a pinned, known-good version
- **Latest release** — the latest stable release (default)
- **Dev/nightly** — development or pre-release builds

All three channels are target-state. Currently only Hardcoded is operational.

Channel selection is via UI picker. Always show the channel picker. If the selected channel is unavailable, silently present Latest as the active channel. The selected channel is stored in memento (extension state).

The channel affects **both** automatic fallback downloads and manual (UI-triggered) downloads.

After selecting a channel, the UI shows native/WASM options (on desktop).

## `PreferredCandidate`

A user's preferred candidate, stored as an independent value — not a pointer into `connection.candidates`.

- Stores a candidate value (filepath, command name, or URI), independent from the candidate list.
- Only set by explicit user action — never by the system.
  - **Switch Version UI**: user picks a candidate.
  - **Manual download**: user triggers a download via UI; on success, set to the downloaded path.

When set, the preferred candidate is always tried first, regardless of whether it appears in `connection.candidates`.

- If it **succeeds**: use it. Don't modify the list.
- If it **fails** (for any reason — missing, broken, stale): **keep the preferred candidate**, fall through silently to the normal reverse-order candidate walk.
- Only cleared by explicit user action (Switch Version UI or similar).

## Q&A

1. **Defaults in config or runtime?** — Defaults are materialized in `package.json`. Default value is `[“agda”, “als”]`.

2. **Empty list behavior?** — With defaults in `package.json`, an empty list means no candidates; falls through directly to download fallback.

3. **URIs as candidates?** — `file://` URIs are accepted and normalized to platform-specific paths. WASM paths keep URI scheme.

4. **Downloads highest priority forever?** — Depends on type. Automatic downloads prepend (lowest priority). Manual downloads append (highest priority).

5. **Issue #272 regressions?** — Normalization must use resolved forms for comparison, not plain strings.

6. **Stale PreferredCandidate?** — Keep the preferred candidate. Fall through silently to normal candidate walk. If it later reappears or works, it'll just work again.

7. **PreferredCandidate failure fallback?** — Keep the preferred candidate. Fall through silently to normal reverse-order walk. Preserves user intent across transient failures.

8. **System auto-setting preference?** — No. Only explicit user action sets PreferredCandidate. Code needs fix.

9. **Channel scope?** — Stored in memento (target-state; not yet implemented).

10. **All three channels implemented?** — Target-state. Currently only Hardcoded is operational.

11. **Channel affects automatic fallback?** — Yes, channel determines which version is downloaded for both auto and manual downloads.

12. **Cleanup policy?** — “Delete Downloads” UI is the only mechanism. Same rules for auto and manual. No automatic cleanup.

13. **Normalization contract?** — Platform-specific canonicalization. Desktop: tilde expansion, path normalization, absolute resolution, `file://` to fsPath. Web: paths keep URI scheme. Comparison uses normalized forms.

14. **Native→WASM fallback per channel?** — Yes, on desktop if native fails, WASM is attempted automatically for the same channel.

15. **Both native and WASM downloaded?** — Native checked first. WASM only if native not found. Deterministic.

16. **UI picker hidden when <2 channels?** — No. Always show the channel picker.

17. **Migration for paths→candidates rename?** — Straight rename. Old setting gets `deprecationMessage`, no fallback/switching.

18. **Migration for stale memento key?** — Memento key change effectively clears it. No explicit migration needed.

19. **Canonical storage format for WASM candidates?** — Platform-dependent. On desktop, all paths are normalized to fsPath. On web, WASM paths keep their URI scheme (no fsPath available).

20. **Ban writing command-resolved paths back?** — Yes. Command-resolved absolute paths must never be written back into candidates. Resolved paths are transient (connection-time only).

21. **One-option channel picker intentional?** — Show all three channels. Disable Latest/Dev as “coming soon” until operational.

22. **Mark document as target-state spec?** — Yes. Document is marked as target-state spec at the top.

23. **Both old and new settings present?** — New setting (`connection.candidates`) wins. Old setting is ignored if new is present.

24. **Losing user preference on rename acceptable?** — Implicit clear is acceptable. No one-time migration needed. User can re-pick.

25. **PreferredCandidate failure warning noise?** — No warning. Silent fall-through to normal candidate walk.

26. **URI as first-class candidate type?** — Yes. URI is a first-class candidate type alongside filepath and command name.

27. **PreferredCandidate allow URI values?** — Yes. PreferredCandidate allows URI values — consistent with the candidate definition (filepath, command name, or URI).

28. **Normalization contract platform-specific?** — Yes. Canonicalization is platform-specific: desktop normalizes to fsPath; web keeps URI scheme. Stated as one rule in Q13.

29. **Stale memento with disabled channel?** — Fall back to Latest. Latest is the default channel. If the chosen channel fails, silently present Latest instead. When the original channel becomes operational, it activates automatically.

30. **Promote Q&A rules to normative sections?** — No. Q&A entries are authoritative. No promotion needed.

31. **Q&A vs main-section conflict rule?** — Q&A wins. Q&A entries are authoritative over main sections. If a future edit in main sections disagrees with Q&A, the main section must be updated to match.

32. **Delete Downloads and PreferredCandidate?** — Keep stale. PreferredCandidate stays as-is when Delete Downloads removes its target. It will fail at connection time and silently fall through (consistent with Q6/Q7).

33. **Command name vs resolved path duplicates?** — Always distinct. Command names and resolved paths are different candidate types. No semantic deduplication across types.

34. **Stale disabled channel UI display?** — Show Latest. If the chosen channel is unavailable, silently present Latest as the active channel (consistent with Q29). No greyed/disabled state needed.

35. **Effective channel badge for failed channel?** — No. Latest is silently presented when the chosen channel fails (per Q29/Q34). No badge or extra indicator needed.

36. **UI affordance for stale PreferredCandidate?** — No affordance. Silent fall-through is sufficient (consistent with Q6/Q7/Q25). User can always re-pick manually.

37. **CI enforcement for Q&A vs main-section drift?** — No enforcement. Q&A authority rule (Q31) is sufficient. Drift is caught in manual review.