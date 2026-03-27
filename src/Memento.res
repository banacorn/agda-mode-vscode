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

  module ResolvedMetadata: {
    let entries: t => Dict.t<Endpoints.entry>
    let get: (t, Connection__Candidate.Resolved.t) => option<Endpoints.entry>
    let setVersion: (t, Connection__Candidate.Resolved.t, Endpoints.endpoint) => promise<unit>
    let setError: (t, Connection__Candidate.Resolved.t, string) => promise<unit>
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

  module SelectedChannel: {
    let get: t => option<string>
    let set: (t, string) => promise<unit>
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
    module Candidate = Connection__Candidate

    type candidateKey = string // raw candidate string provided by the user or discovered at runtime
    // what kind of endpoint the candidate leads to?
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

    let get = (memento: t, candidateKey: candidateKey): option<entry> => {
      let cache = memento->getWithDefault(key, Dict.make())
      cache->Dict.get(candidateKey)
    }

    let lookupCandidateInCache = (
      cache: Dict.t<entry>,
      candidateKey: candidateKey,
    ): option<(candidateKey, entry)> => {
      switch cache->Dict.get(candidateKey) {
      | Some(entry) => Some((candidateKey, entry))
      | None =>
        let candidate = Candidate.make(candidateKey)
        cache
        ->Dict.toArray
        ->Array.findMap(((existingKey, entry)) =>
          if Candidate.equal(Candidate.make(existingKey), candidate) {
            Some((existingKey, entry))
          } else {
            None
          }
        )
      }
    }

    let lookupCandidate = (
      memento: t,
      candidateKey: candidateKey,
    ): option<(candidateKey, entry)> => {
      let cache = memento->getWithDefault(key, Dict.make())
      lookupCandidateInCache(cache, candidateKey)
    }

    let setVersion = async (memento: t, candidateKey: candidateKey, endpoint: endpoint): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let entry = {endpoint, timestamp: Date.make(), error: None}
      cache->Dict.set(candidateKey, entry)
      await memento->set(key, cache)
    }

    let setError = async (memento: t, candidateKey: candidateKey, error: string): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let existingEndpoint = switch cache->Dict.get(candidateKey) {
      | Some(existingEntry) => existingEntry.endpoint
      | None => Unknown
      }
      let entry = {endpoint: existingEndpoint, timestamp: Date.make(), error: Some(error)}
      cache->Dict.set(candidateKey, entry)
      await memento->set(key, cache)
    }

    let updateCandidate = async (
      memento: t,
      candidateKey: candidateKey,
      makeEntry: option<entry> => entry,
    ): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let (keyToUpdate, existingEntry) = switch lookupCandidateInCache(cache, candidateKey) {
      | Some((existingKey, existingEntry)) => (existingKey, Some(existingEntry))
      | None => (candidateKey, None)
      }
      let entry = makeEntry(existingEntry)
      cache->Dict.set(keyToUpdate, entry)
      await memento->set(key, cache)
    }

    let setVersionForCandidate = async (
      memento: t,
      candidateKey: candidateKey,
      endpoint: endpoint,
    ): unit => {
      await updateCandidate(memento, candidateKey, _existingEntry => {
        endpoint,
        timestamp: Date.make(),
        error: None,
      })
    }

    let setErrorForCandidate = async (
      memento: t,
      candidateKey: candidateKey,
      error: string,
    ): unit => {
      await updateCandidate(memento, candidateKey, existingEntry => {
        let endpoint = switch existingEntry {
        | Some(existingEntry) => existingEntry.endpoint
        | None => Unknown
        }
        {endpoint, timestamp: Date.make(), error: Some(error)}
      })
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

  module ResolvedMetadata = {
    module Resolved = Connection__Candidate.Resolved

    let key = "resolvedMetadata"

    let entries = (memento: t): Dict.t<Endpoints.entry> =>
      switch memento {
      | Memento(memento) => VSCode.Memento.getWithDefault(memento, key, Dict.make())
      | Mock(dict) =>
        switch Dict.get(dict, key) {
        | Some(value) => value->Obj.magic
        | None => Dict.make()
        }
      }

    let get = (memento: t, resolved: Resolved.t): option<Endpoints.entry> => {
      let cache = memento->getWithDefault(key, Dict.make())
      cache->Dict.get(Resolved.toString(resolved))
    }

    let setVersion = async (
      memento: t,
      resolved: Resolved.t,
      endpoint: Endpoints.endpoint,
    ): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let entry: Endpoints.entry = {endpoint, timestamp: Date.make(), error: None}
      cache->Dict.set(Resolved.toString(resolved), entry)
      await memento->set(key, cache)
    }

    let setError = async (memento: t, resolved: Resolved.t, error: string): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let resolvedKey = Resolved.toString(resolved)
      let existingEndpoint = switch cache->Dict.get(resolvedKey) {
      | Some(existingEntry) => existingEntry.endpoint
      | None => Endpoints.Unknown
      }
      let entry: Endpoints.entry = {
        endpoint: existingEndpoint,
        timestamp: Date.make(),
        error: Some(error),
      }
      cache->Dict.set(resolvedKey, entry)
      await memento->set(key, cache)
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

  module SelectedChannel = {
    let key = "selectedChannel"
    let mementoSet = set
    let mementoGet = get

    let get = (memento: t): option<string> => {
      memento->mementoGet(key)
    }

    let set = async (memento: t, channel: string): unit => {
      await memento->mementoSet(key, Some(channel))
    }

    let clear = async (memento: t): unit => {
      await memento->mementoSet(key, None)
    }
  }
}

include Module
