type ownerId = string

module Resource = {
  type t = {
    connection: Connection.t,
    mutable users: Belt.Set.String.t,
    mutable currentOwnerId: option<ownerId>,
    mutable queue: promise<unit>,
  }
}

type status =
  | Empty
  | Connecting(promise<result<Connection.t, Connection.Error.t>>)
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
    // Util.log("Registry__Connection: Terminating connection", ())
    let _ = await Connection.destroy(Some(resource.connection), Chan.make())
    status := Empty
    // Reset makeCount for testing isolation
    makeCount := 0
    resolve()
  }
}

//
// Lifecycle API
//

// Acquire: Join the global connection or initiate it if Empty.
let rec acquire: (
  ownerId,
  unit => promise<result<Connection.t, Connection.Error.t>>,
) => promise<result<Connection.t, Connection.Error.t>> = async (id, make) => {
  let connect = async () => {
    let (connectionEstablished, resolve, _) = Util.Promise_.pending()
    status := Connecting(connectionEstablished)
    // Util.log("Registry__Connection: " ++ id ++ " is initiating connection", ())
    
    // Increment the process creation counter
    makeCount := makeCount.contents + 1
    
    let result = await make()
    switch result {
    | Ok(connection) =>
      let resource: Resource.t = {
        connection: connection,
        users: Belt.Set.String.fromArray([id]),
        currentOwnerId: None,
        queue: Promise.resolve(),
      }
      status := Active(resource)
    | Error(_) => status := Empty
    }
    resolve(result)
    result
  }

  let wait = async connectionEstablished => {
    let result = await connectionEstablished
    switch result {
    | Ok(_connection) =>
      // Once connected, the state should be Active
      switch status.contents {
      | Active(resource) =>
        resource.users = resource.users->Belt.Set.String.add(id)
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
  | Connecting(connectionEstablished) => await wait(connectionEstablished)
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
  | Connecting(connectionEstablished) =>
    let _ = await connectionEstablished
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
  Connection.t => promise<result<'a, Connection.Error.t>>,
) => promise<result<'a, Connection.Error.t>> = async (id, task) => {
  let runSerialized = async (resource: Resource.t) => {
    let previousTaskDone = resource.queue
    let (currentTaskDone, resolve, _) = Util.Promise_.pending()
    resource.queue = currentTaskDone

    await previousTaskDone
    resource.currentOwnerId = Some(id)
    let result = await task(resource.connection)
    resource.currentOwnerId = None
    resolve()
    result
  }

  switch status.contents {
  | Active(resource) =>
    if resource.currentOwnerId == Some(id) {
      await task(resource.connection)
    } else {
      await runSerialized(resource)
    }
  | _ =>
    let err = Connection.Error.Establish(Connection.Error.Establish.mergeMany([]))
    Error(err)
  }
}
