module Core = Connection__Core

type ownerId = string

module Resource = {
  type t = {
    connection: Core.t,
    mutable users: Belt.Set.String.t,
    mutable currentOwnerId: option<ownerId>,
    mutable queue: promise<unit>,
  }
}

module Connecting = {
  type t = {
    connectionEstablished: promise<result<Core.t, Core.Error.t>>,
    mutable releasedUsers: Belt.Set.String.t,
  }
}

module ExecutionContext = {
  type storage

  let storage: Js.Nullable.t<storage> = %raw(`(() => {
    if (typeof require !== "function") {
      return null;
    }
    try {
      const { AsyncLocalStorage } = require("node:async_hooks");
      return new AsyncLocalStorage();
    } catch (_) {
      return null;
    }
  })()`)

  @send
  external runWithOwner: (
    storage,
    ownerId,
    unit => promise<'a>,
  ) => promise<'a> = "run"

  @send external currentOwnerNullable: storage => Js.Nullable.t<ownerId> = "getStore"

  let withOwner = (id, task) =>
    switch Js.Nullable.toOption(storage) {
    | Some(storage) => storage->runWithOwner(id, task)
    | None => task()
    }

  let isOwnerActive = id =>
    switch Js.Nullable.toOption(storage) {
    | Some(storage) => storage->currentOwnerNullable->Js.Nullable.toOption == Some(id)
    | None => false
    }

  let hasAsyncOwnerContext = () =>
    switch Js.Nullable.toOption(storage) {
    | Some(_) => true
    | None => false
    }
}

type status =
  | Empty
  | Connecting(Connecting.t)
  | Active(Resource.t)
  | Closing(promise<unit>)

// The global singleton state for this window.
// Transitions are atomic because state updates occur synchronously before the first 'await'.
let status: ref<status> = ref(Empty)

// Counter for verification to track how many times a connection was actually established
let makeCount = ref(0)

//
// Observability (Scaffolding for Verification)
//

type view = {
  status: string,
  userCount: int,
  currentOwnerId: option<ownerId>,
  makeCount: int,
}

let inspect = (): view => {
  switch status.contents {
  | Empty => {status: "Empty", userCount: 0, currentOwnerId: None, makeCount: makeCount.contents}
  | Connecting(_) => {status: "Connecting", userCount: 0, currentOwnerId: None, makeCount: makeCount.contents}
  | Active(resource) => {
      status: "Active",
      userCount: resource.users->Belt.Set.String.size,
      currentOwnerId: resource.currentOwnerId,
      makeCount: makeCount.contents,
    }
  | Closing(_) => {status: "Closing", userCount: 0, currentOwnerId: None, makeCount: makeCount.contents}
  }
}

//
// Lifecycle Helpers
//

let terminate = async (resource: Resource.t) => {
  switch status.contents {
  | Closing(connectionDestroyed) => await connectionDestroyed
  | _ =>
    let (connectionDestroyed, resolve, _) = Util.Promise_.pending()
    status := Closing(connectionDestroyed)
    let destroyError = ref(None)
    // Util.log("Registry__Connection: Terminating connection", ())
    let _ = switch await Core.destroy(Some(resource.connection), Chan.make()) {
    | _ => ()
    | exception exn =>
      destroyError := Some(exn)
      ()
    }
    status := Empty
    // Reset makeCount for testing isolation
    makeCount := 0
    resolve()
    destroyError.contents->Option.forEach(exn => raise(exn))
  }
}

//
// Lifecycle API
//

// Acquire: Join the global connection or initiate it if Empty.
let rec acquire: (
  ownerId,
  unit => promise<result<Core.t, Core.Error.t>>,
) => promise<result<Core.t, Core.Error.t>> = async (id, make) => {
  let connect = async () => {
    let (connectionEstablished, resolve, _) = Util.Promise_.pending()
    let connecting: Connecting.t = {connectionEstablished, releasedUsers: Belt.Set.String.empty}
    status := Connecting(connecting)
    // Util.log("Registry__Connection: " ++ id ++ " is initiating connection", ())

    let result = switch await make() {
    | result => result
    | exception _ =>
      Error(Core.Error.Establish(Core.Error.Establish.mergeMany([])))
    }

    switch result {
    | Ok(connection) =>
      // Increment only when a connection was actually established
      makeCount := makeCount.contents + 1
      let users = if connecting.releasedUsers->Belt.Set.String.has(id) {
        Belt.Set.String.empty
      } else {
        Belt.Set.String.fromArray([id])
      }
      let resource: Resource.t = {
        connection: connection,
        users,
        currentOwnerId: None,
        queue: Promise.resolve(),
      }
      status := Active(resource)
    | Error(_) => status := Empty
    }
    resolve(result)
    result
  }

  let wait = async (connecting: Connecting.t) => {
    let result = await connecting.connectionEstablished
    switch result {
    | Ok(_connection) =>
      // Once connected, the state should be Active
      switch status.contents {
      | Active(resource) =>
        if !(connecting.releasedUsers->Belt.Set.String.has(id)) {
          resource.users = resource.users->Belt.Set.String.add(id)
        }
        result
      | _ => await acquire(id, make)
      }
    | Error(_) => result
    }
  }

  let join = (resource: Resource.t) => {
    resource.users = resource.users->Belt.Set.String.add(id)
    Promise.resolve(Ok(resource.connection))
  }

  switch status.contents {
  | Empty => await connect()
  | Connecting(connecting) =>
    // Owner is acquiring again, so clear any stale pre-connect release marker.
    connecting.releasedUsers = connecting.releasedUsers->Belt.Set.String.remove(id)
    await wait(connecting)
  | Active(resource) => await join(resource)
  | Closing(connectionDestroyed) =>
    await connectionDestroyed
    await acquire(id, make)
  }
}

// Release: Remove an owner from the connection.
// If the owner was the last one, the connection is moved to Closing then Empty.
let release: ownerId => promise<unit> = async id => {
  switch status.contents {
  | Connecting(connecting) =>
    connecting.releasedUsers = connecting.releasedUsers->Belt.Set.String.add(id)
  | Active(resource) =>
    resource.users = resource.users->Belt.Set.String.remove(id)
    if resource.users->Belt.Set.String.isEmpty {
      await terminate(resource)
    }
  | _ => ()
  }
}

// Shutdown: Forcefully destroy the connection (e.g., on "Restart").
let rec shutdown: unit => promise<unit> = async () => {
  switch status.contents {
  | Active(resource) => await terminate(resource)
  | Connecting(connecting) =>
    let _ = await connecting.connectionEstablished
    await shutdown()
  | Closing(connectionDestroyed) => await connectionDestroyed
  | Empty => ()
  }
}

//
// Execution
//

// Execute: Wrap a connection task with serialization and reentrancy logic.
// This is the core "gate" that prevents interleaved responses.
let execute: (
  ownerId,
  Core.t => promise<result<'a, Core.Error.t>>,
) => promise<result<'a, Core.Error.t>> = async (id, task) => {
  let runSerialized = async (resource: Resource.t) => {
    let previousTaskDone = resource.queue
    let (currentTaskDone, resolve, _) = Util.Promise_.pending()
    resource.queue = currentTaskDone

    let cleanup = () => {
      resource.currentOwnerId = None
      resolve()
    }

    let staleConnectionError = () => {
      let err = Core.Error.Establish(Core.Error.Establish.mergeMany([]))
      Error(err)
    }

    switch await previousTaskDone {
    | _ =>
      switch status.contents {
      | Active(activeResource) if activeResource === resource =>
        resource.currentOwnerId = Some(id)
        switch await ExecutionContext.withOwner(id, () => task(resource.connection)) {
        | result =>
          cleanup()
          result
        | exception exn =>
          cleanup()
          raise(exn)
        }
      | _ =>
        cleanup()
        staleConnectionError()
      }
    | exception exn =>
      cleanup()
      raise(exn)
    }
  }

  switch status.contents {
  | Active(resource) =>
    let canBypassQueue = if ExecutionContext.hasAsyncOwnerContext() {
      resource.currentOwnerId == Some(id) && ExecutionContext.isOwnerActive(id)
    } else {
      // Fallback for environments without async context propagation support.
      resource.currentOwnerId == Some(id)
    }
    if canBypassQueue {
      await task(resource.connection)
    } else {
      await runSerialized(resource)
    }
  | _ =>
    let err = Core.Error.Establish(Core.Error.Establish.mergeMany([]))
    Error(err)
  }
}

// Keep owner context available across callbacks that may run outside the original
// async chain (e.g. event-emitter response handlers).
let withOwnerContext = (id, task) => ExecutionContext.withOwner(id, task)
