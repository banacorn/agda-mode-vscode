# Specification: Window-scoped Connection Sharing

This document outlines the architecture for sharing a single Agda connection (process) across all open files within a VS Code window. This design mimics the efficiency of Emacs `agda2-mode`, leveraging Agda's internal module cache to make subsequent file loads near-instant.

## 1. Core Architecture: The Global Registry
All `State.t` (per-file) instances share a single `Connection.t` managed by a global singleton: `Registry__Connection.res`.

The registry operates on a strict state machine:
`Empty | Connecting(promise) | Active(Resource.t) | Closing(promise)`

```rescript
module Resource = {
  type t = {
    connection: Connection.t,
    mutable users: Belt.Set.String.t, // Set of file IDs currently using the connection
    mutable currentOwnerId: option<string>, // Used for reentrant locking
    mutable queue: promise<unit>, // Serializes concurrent editor requests
  }
}
```

## 2. Synchronization & Reentrancy
Because JavaScript is single-threaded, state transitions (like `Empty` -> `Connecting`) are synchronous and atomic. Subsequent callers attempting to acquire a connection will safely await the existing `Connecting` promise instead of spawning a redundant OS process.

To prevent interleaved command execution from different files, requests are serialized via `resource.queue`. If a file issues a nested command while already holding the lock, the `currentOwnerId` check allows it to bypass the queue (reentrancy).

## 3. Reference Counting & Lifecycle
The connection lifecycle is tied to the `users` set:
- **Spawn**: The first file calls `acquire()`, moving status to `Connecting`.
- **Share**: Subsequent files join the `users` set of the `Active` resource.
- **Destroy**: Files call `release()` on closure. When the `users` set empties, the connection is terminated.
- **Shutdown**: A global command that forcefully clears the registry.

## 4. Teardown & Verification
- **Instant Teardown**: Agda is a stateless typechecker. To ensure $O(1)$ cleanup time without hanging, `Connection.destroy` forcefully kills the process tree (`SIGKILL` / `taskkill`) and resolves immediately, intentionally bypassing graceful asynchronous OS exits.
- **Internal Proofs**: To guarantee connection pooling across the test suite, the registry maintains a `makeCount` state. Tests assert this counter exactly equals `1` across multiple file loads, providing mathematical proof of process sharing without relying on flaky OS-level checks.