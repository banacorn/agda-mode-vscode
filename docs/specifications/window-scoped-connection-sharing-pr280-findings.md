# PR #280 Review Findings: Window-Scoped Connection Sharing

Date: 2026-02-27
Branch reviewed: `pr-280`
Related spec: `docs/specifications/window-scoped-connection-sharing.md`

## Regression Test Tracking

Use two checkboxes per finding:
- `Regression Test Added`: a test exists that exposes the bug on current `pr-280`.
- `Regression Test Fixed`: after implementing the fix, that same test passes.

| ID | Severity | Finding | Test File | Regression Test Added | Regression Test Fixed |
| --- | --- | --- | --- | --- | --- |
| F1 | High | `execute` deadlock on task throw/reject | `test/tests/Test__Registry__Connection.res` | [x] | [x] |
| F2 | High | `acquire` deadlock if `make()` throws/rejects (and `shutdown` hang impact) | `test/tests/Test__Registry__Connection.res` | [x] | [x] |
| F3 | High | `terminate` deadlock if `Connection.destroy` throws/rejects | `test/tests/Test__Registry__Connection.res` | [x] | [x] |
| F4 | High | `release` ignores `Connecting` and leaks phantom users | `test/tests/Test__Registry__Connection.res` | [x] | [x] |
| F5 | Medium | `runSerialized` can run with stale/destroying connection after queue wait | `test/tests/Test__Registry__Connection.res` | [ ] | [ ] |
| F6 | Medium | switch-version tears down old connection before new is proven | `test/tests/Test__State__SwitchVersion.res` | [ ] | [ ] |
| F7 | Medium | process destroy resolves before exit confirmation | `test/tests/Connection/Test__Connection__Process.res` | [ ] | [ ] |
| F8 | Low | process destroy state machine never stabilizes at `Destroyed` | `test/tests/Connection/Test__Connection__Process.res` | [ ] | [ ] |
| F9 | Medium | same-owner fast-path allows concurrent (not only nested) execution | `test/tests/Test__Registry__Connection.res` | [ ] | [ ] |

## Findings Detail

### F1 (High): `execute` can deadlock the registry on throw/reject
- File: `src/Registry__Connection.res:167-177`
- Issue: `currentOwnerId` is cleared and queue is resolved only on the happy path. If `task(...)` throws/rejects, both cleanup steps are skipped.
- Impact: all subsequent requests can block forever for that window.
- Suggested fix: wrap task execution in `try/finally` so owner/queue cleanup always runs.

### F2 (High): `acquire` can deadlock if `make()` throws/rejects
- File: `src/Registry__Connection.res:78-99`
- Issue: status transitions to `Connecting` before `await make()`. If `make()` rejects/throws instead of returning `Error(...)`, the pending promise is never resolved and status is never reset.
- Impact: future `acquire` calls can wait forever. `shutdown()` can also hang forever on the `Connecting` branch (`await connectionEstablished`), which can block restart/switch flows.
- Additional observability impact: `makeCount` is incremented before `await make()`, so a thrown `make()` can inflate `inspect().makeCount` without any successful connection establishment.
- Suggested fix: guard `make()` with `try/catch/finally`, always resolving the pending promise and restoring state on failure.

### F3 (High): `terminate` can deadlock if `Connection.destroy` throws/rejects
- File: `src/Registry__Connection.res:54-66`
- Issue: status is set to `Closing(connectionDestroyed)` before `await Connection.destroy(...)`. If destroy throws/rejects, status reset and `resolve()` are skipped.
- Impact: registry can be stuck in `Closing` forever; future `acquire`/`release`/`shutdown` calls can block. In particular, `acquire`'s `Closing` branch (`await connectionDestroyed`) can deadlock permanently.
- Suggested fix: guard teardown with `try/catch/finally` and always resolve/reset `Closing` state.

### F4 (High): `release` ignores `Connecting`, causing phantom users
- Files: `src/Registry__Connection.res:102-110`, `src/Registry__Connection.res:132-141`
- Issue: if an owner closes while status is `Connecting`, `release` is a no-op; after connect completes, `wait` still adds that owner to `users`.
- Impact: leaked user references can prevent connection shutdown.
- Suggested fix: track and honor releases that occur during `Connecting` (or make join conditional on liveness).

### F5 (Medium): `runSerialized` can run with stale/destroying connection after queue wait
- File: `src/Registry__Connection.res:172-174`
- Issue: after `await previousTaskDone`, execution proceeds using captured `resource.connection` without re-checking `status`/resource identity. If teardown started while waiting, queued work can run against a connection being destroyed.
- Impact: requests may run on invalid connection state and fail unpredictably; if task throws/rejects here, deadlock risk from F1 can cascade.
- Suggested fix: re-check that registry is still `Active` with the same resource after queue wait; otherwise return a connection-establish error.

### F6 (Medium): switch-version tears down old connection before new connection is proven
- File: `src/State/State__SwitchVersion.res:438-475`
- Issue: `Registry__Connection.shutdown()` runs before `Connection.make(path, ...)` succeeds.
- Impact: switching to a broken target can drop a previously working session.
- Suggested fix: stage new connection first, then atomically swap/teardown old connection.

### F7 (Medium): process destroy now resolves before exit confirmation
- File: `src/Connection/Transport/Connection__Transport__Process.res:116-147`
- Issue: destroy force-kills and resolves immediately; Windows `taskkill` failures are swallowed; no wait for `OnExit`.
- Impact: callers may assume process is gone when it may still be alive, causing restart races (ports/pipes/process handles).
- Suggested fix: keep immediate-kill if desired, but provide a reliable "confirmed exited" path (or stricter failure signaling).

### F8 (Low): process destroy state machine never stabilizes at `Destroyed`
- File: `src/Connection/Transport/Connection__Transport__Process.res:123,143`
- Issue: `self.status = Destroyed` is assigned inside the promise executor, but immediately overwritten by `self.status = Destroying(promise)` after promise creation.
- Impact: low runtime impact now, but state modeling is misleading and can confuse future lifecycle logic.
- Suggested fix: set `Destroyed` as the stable terminal state after cleanup, or rename/restructure states to match actual behavior.

### F9 (Medium): same-owner fast-path allows concurrent (not only nested) execution
- File: `src/Registry__Connection.res:181-185`
- Issue: `currentOwnerId == Some(id)` bypasses queueing for same owner even when calls are sibling-concurrent rather than true nested reentrancy.
- Impact: responses can interleave and defeat strict serialization.
- Suggested fix: either serialize same-owner concurrent calls too, or explicitly document/test that this is allowed.

## Open Questions

- Should same-owner bypass permit only true nested reentrancy, or also concurrent sibling requests?
- Is "destroy-now without waiting for exit" an explicit product decision for restart latency, or should shutdown preserve stronger completion guarantees?
