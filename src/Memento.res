module Any: {
  type t
} = {
  type t
}

// Binds to VSCode.Memento when VSCode.ExtensionContext is available
// Binds to a mock when testing
module Module: {
  type t
  // constructor
  let make: option<VSCode.Memento.t> => t
  // for debugging
  let toString: t => string

  module Endpoints: {
    type filepath = string // raw file path
    type endpoint =
      | Agda(option<string>) // Agda version
      | ALS(
          option<(
            string,
            string,
            option<Connection__Protocol__LSP__Binding.executableOptions>,
          )>,
        ) // ALS version & corresponding Agda version & LSP options
      | Unknown
    let endpointToString: endpoint => string

    type entry = {
      endpoint: endpoint,
      timestamp: Date.t,
      error: option<string>,
    }
    let entries: t => Dict.t<entry>
    let get: (t, filepath) => option<entry>
    let setVersion: (t, filepath, endpoint) => promise<unit>
    let setError: (t, filepath, string) => promise<unit>
    let syncWithPaths: (t, Dict.t<endpoint>) => promise<unit>
    let clear: t => promise<unit>
  }

  module ALSReleaseCache: {
    let getTimestamp: (t, string, string) => option<Date.t>
    let setTimestamp: (t, string, string, Date.t) => promise<unit>
    let getReleases: (t, string, string) => option<'releases>
    let setReleases: (t, string, string, 'releases) => promise<unit>
    let getCacheAgeInSecs: (t, string, string) => option<int>
    let clear: (t, string, string) => promise<unit>
  }

  module PickedConnection: {
    let get: t => option<string>
    let set: (t, option<string>) => promise<unit>
    let clear: t => promise<unit>
  }
} = {
  type t = Memento(VSCode.Memento.t) | Mock(Dict.t<Any.t>)

  let make = memento =>
    switch memento {
    | Some(memento) => Memento(memento)
    | None => Mock(Dict.make())
    }

  let get = (context, key) =>
    switch context {
    | Memento(context) => VSCode.Memento.get(context, key)
    | Mock(dict) => Obj.magic(dict->Dict.get(key))
    }
  let getWithDefault = (context, key, defaultValue) =>
    switch context {
    | Memento(context) => VSCode.Memento.getWithDefault(context, key, defaultValue)
    | Mock(dict) =>
      switch dict->Dict.get(key) {
      | Some(value) => Obj.magic(value)
      | None => defaultValue
      }
    }

  let set = (context, key, value) =>
    switch context {
    | Memento(context) => VSCode.Memento.update(context, key, value)
    | Mock(dict) => dict->Dict.set(key, Obj.magic(value))->Promise.resolve
    }

  let toString = context =>
    switch context {
    | Memento(context) =>
      let entries = VSCode.Memento.keys(context)->Array.map(key => {
        switch VSCode.Memento.get(context, key) {
        | None => key ++ ": None"
        | Some(value) => key ++ ": " ++ value
        }
      })
      "Memento: {\n" ++ Array.join(entries, "\n") ++ "}"
    | Mock(dict) =>
      let entries =
        dict
        ->Dict.toArray
        ->Array.map(((key, value)) => {
          key ++ ": " ++ value->Obj.magic->Js.String.make
        })
      "Mock: {\n" ++ Array.join(entries, "\n") ++ "}"
    }

  module Endpoints = {
    type filepath = string // raw file path provided by the user or found in the system
    // what kind of endpoint the file path leads to?
    type endpoint =
      | Agda(option<string>) // Agda version
      | ALS(
          option<(
            string,
            string,
            option<Connection__Protocol__LSP__Binding.executableOptions>,
          )>,
        ) // ALS version & corresponding Agda version & LSP options
      | Unknown
    let endpointToString = endpoint =>
      switch endpoint {
      | Agda(Some(version)) => "Agda v" ++ version
      | Agda(None) => "Agda (version unknown)"
      | ALS(Some((alsVersion, agdaVersion, _lspOptions))) =>
        "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion
      | ALS(None) => "Agda Language Server (version unknown)"
      | Unknown => "Unknown"
      }

    type entry = {
      endpoint: endpoint,
      timestamp: Date.t,
      error: option<string>,
    }

    let key = "endpointVersion"

    let entries = (memento: t): Dict.t<entry> =>
      switch memento {
      | Memento(memento) => VSCode.Memento.getWithDefault(memento, key, Dict.make())
      | Mock(dict) =>
        switch Dict.get(dict, key) {
        | Some(value) => value->Obj.magic
        | None => Dict.make()
        }
      }

    let get = (memento: t, filepath: filepath): option<entry> => {
      let cache = memento->getWithDefault(key, Dict.make())
      cache->Dict.get(filepath)
    }

    let setVersion = async (memento: t, filepath: filepath, endpoint: endpoint): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let entry = {endpoint, timestamp: Date.make(), error: None}
      cache->Dict.set(filepath, entry)
      await memento->set(key, cache)
    }

    let setError = async (memento: t, filepath: filepath, error: string): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let existingEndpoint = switch cache->Dict.get(filepath) {
      | Some(existingEntry) => existingEntry.endpoint
      | None => Unknown
      }
      let entry = {endpoint: existingEndpoint, timestamp: Date.make(), error: Some(error)}
      cache->Dict.set(filepath, entry)
      await memento->set(key, cache)
    }

    let syncWithPaths = async (memento: t, discoveredEndpoints: Dict.t<endpoint>): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let newCache = Dict.make()

      // Add entries for all discovered paths
      discoveredEndpoints
      ->Dict.toArray
      ->Array.forEach(((path, discoveredEndpoint)) => {
        switch cache->Dict.get(path) {
        | Some(existingEntry) =>
          // Update endpoint type if we have better inference, but preserve version info and errors
          let updatedEndpoint = switch (existingEntry.endpoint, discoveredEndpoint) {
          | (Unknown, newType) if newType != Unknown => // Update from Unknown to a specific type
            newType
          | (existingType, _) => // Keep existing type (it has version info or is already specific)
            existingType
          }
          let updatedEntry = {...existingEntry, endpoint: updatedEndpoint, timestamp: Date.make()}
          newCache->Dict.set(path, updatedEntry)
        | None =>
          // Add new path with inferred endpoint type
          let entry = {endpoint: discoveredEndpoint, timestamp: Date.make(), error: None}
          newCache->Dict.set(path, entry)
        }
      })

      // Remove entries for paths that no longer exist (cleanup)
      // This happens automatically since we only add current paths to newCache

      await memento->set(key, newCache)
    }
    
    let clear = async (memento: t): unit => {
      await memento->set(key, Dict.make())
    }
  }

  module ALSReleaseCache = {
    let makeTimestampKey = (username, repository) => "alsReleaseCacheTimestamp_" ++ username ++ "/" ++ repository
    let makeReleasesKey = (username, repository) => "alsReleaseCache_" ++ username ++ "/" ++ repository

    let getTimestamp = (memento: t, username, repository): option<Date.t> => {
      let key = makeTimestampKey(username, repository)
      memento->get(key)->Option.map(Date.fromString)
    }

    let setTimestamp = async (memento: t, username, repository, timestamp: Date.t): unit => {
      let key = makeTimestampKey(username, repository)
      await memento->set(key, Date.toString(timestamp))
    }

    let getReleases = (memento: t, username, repository): option<'releases> => {
      let key = makeReleasesKey(username, repository)
      memento->get(key)
    }

    let setReleases = async (memento: t, username, repository, releases: 'releases): unit => {
      let key = makeReleasesKey(username, repository)
      await memento->set(key, releases)
    }

    // return the time difference in seconds since the cache was last fetched
    let getCacheAgeInSecs = (memento: t, username, repository): option<int> => {
      switch getTimestamp(memento, username, repository) {
      | None => None
      | Some(timestamp) =>
        let now = Date.make()
        let ageInMs = Date.getTime(now) -. Date.getTime(timestamp)
        Some(int_of_float(ageInMs /. 1000.0))
      }
    }
    
    // clear the release cache by removing both timestamp and releases for specific repo
    let clear = async (memento: t, username, repository): unit => {
      let timestampKey = makeTimestampKey(username, repository)
      let releasesKey = makeReleasesKey(username, repository)
      await memento->set(timestampKey, None)
      await memento->set(releasesKey, None)
    }
  }

  module PickedConnection = {
    let key = "pickedConnection"

    let get = (memento: t): option<string> => {
      memento->get(key)
    }

    let set = async (memento: t, path: option<string>): unit => {
      await memento->set(key, path)
    }
    
    let clear = async (memento: t): unit => {
      await set(memento, None)
    }
  }
}

include Module
