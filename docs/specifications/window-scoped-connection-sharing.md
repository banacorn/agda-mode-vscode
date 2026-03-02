# Specification: Window-scoped Connection Sharing

This document describes how one VS Code window shares a single Agda/ALS connection across multiple files.

## 1. Core Architecture
All per-file `State.t` instances share one global registry in `src/Registry__Connection.res`.

Registry state machine:
- `Empty`
- `Connecting(Connecting.t)`
- `Active(Resource.t)`
- `Closing(promise<unit>)`

```rescript
module Resource = {
  type t = {
    connection: Connection.t,
    mutable users: Belt.Set.String.t,
    mutable currentOwnerId: option<string>,
    mutable queue: promise<unit>,
  }
}

module Connecting = {
  type t = {
    connectionEstablished: promise<result<Connection.t, Connection.Error.t>>,
    mutable releasedUsers: Belt.Set.String.t,
  }
}
```

## 2. Lifecycle Semantics
- `acquire` from `Empty` transitions to `Connecting` and creates exactly one in-flight connection establishment.
- `acquire` from `Connecting` awaits the existing `connectionEstablished` promise.
- `release` during `Connecting` is tracked via `releasedUsers`, so owners that close early are not later added as active users.
- `release` from `Active` removes an owner; when `users` is empty, the registry terminates the shared connection.
- `shutdown` handles all states (`Active`, `Connecting`, `Closing`, `Empty`) and eventually converges to `Empty`.

## 3. Serialization and Reentrancy
All command execution goes through the registry gate:
- Requests are serialized with `resource.queue`.
- Cleanup (`currentOwnerId` reset + queue resolution) runs for both success and failure paths.
- After yielding to the queue, execution re-checks that the registry is still `Active` on the same `resource` before using the connection.

Nested same-owner reentrancy is supported:
- On Node runtimes with async context support, owner identity is propagated with `AsyncLocalStorage`.
- Queue bypass is allowed only when the current async owner context matches the active owner, preventing sibling-concurrent same-owner bypass.

## 4. Teardown Semantics
Registry teardown (`terminate`):
- Moves to `Closing` while destroying.
- Always resolves the closing promise and resets to `Empty`, even if `Connection.destroy` throws.
- Re-raises destroy exceptions after state cleanup to avoid deadlocked callers.

Process teardown (`src/Connection/Transport/Connection__Transport__Process.res`):
- Force-kills process tree (`SIGKILL` / `taskkill`).
- `destroy()` resolves only after process exit/close is confirmed (`promiseOnExit`).
- Final stable status after destroy is `Destroyed`.

Design rationale:
- Agda/ALS processes are treated as disposable worker processes, so teardown prioritizes bounded shutdown latency by issuing force-kill.
- Unlike the previous "resolve immediately" behavior, callers now get a stronger lifecycle guarantee because `destroy()` completes only after exit confirmation.

## 5. Switch-version Semantics
`switchAgdaVersion` in `src/State/State__SwitchVersion.res`:
- Proves target connection first (`Connection.make`).
- Shuts down existing shared registry connection only after target is known connectable.
- Updates memento/config and cleans up temporary connection used for probing.

This avoids dropping a working session when switching to an invalid target.

## 6. Verification Coverage
Primary behavior coverage lives in:
- `test/tests/Test__Registry__Connection.res`
- `test/tests/Connection/Test__Connection__Process.res`
- `test/tests/Test__State__SwitchVersion.res`

These tests verify:
- queue recovery after task failures
- acquire/shutdown recovery after connection establishment failure
- terminate recovery after destroy failures
- connecting-time release handling
- same-owner serialization behavior
- process destroy completion semantics and stable status
- switch-version safety when target connection fails
