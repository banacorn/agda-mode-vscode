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

  module ResolvedMetadata: {
    type kind =
      | Agda(option<string>)
      | ALS(
          option<(
            string,
            string,
            option<Connection__Protocol__LSP__Binding.executableOptions>,
          )>,
        )
      | Unknown
    let kindToString: kind => string
    type entry = {
      kind: kind,
      timestamp: Date.t,
      error: option<string>,
    }
    let entries: t => Dict.t<entry>
    let get: (t, Connection__Candidate.Resolved.t) => option<entry>
    let setKind: (t, Connection__Candidate.Resolved.t, kind) => promise<unit>
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

  module ResolvedMetadata = {
    module Resolved = Connection__Candidate.Resolved

    type kind =
      | Agda(option<string>) // Agda version
      | ALS(
          option<(
            string,
            string,
            option<Connection__Protocol__LSP__Binding.executableOptions>,
          )>,
        ) // ALS version & corresponding Agda version & LSP options
      | Unknown
    let kindToString = kind =>
      switch kind {
      | Agda(Some(version)) => "Agda v" ++ version
      | Agda(None) => "Agda (version unknown)"
      | ALS(Some((alsVersion, agdaVersion, _lspOptions))) =>
        "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion
      | ALS(None) => "Agda Language Server (version unknown)"
      | Unknown => "Unknown"
      }

    type entry = {
      kind: kind,
      timestamp: Date.t,
      error: option<string>,
    }

    let key = "resolvedCandidateMetadata"
    let legacyKey = "resolvedMetadata"

    let readEntries = (memento: t, key: string): Dict.t<entry> =>
      switch memento {
      | Memento(memento) => VSCode.Memento.getWithDefault(memento, key, Dict.make())
      | Mock(dict) =>
        switch Dict.get(dict, key) {
        | Some(value) => value->Obj.magic
        | None => Dict.make()
        }
      }

    let writeEntries = async (memento: t, cache: Dict.t<entry>): unit => {
      await memento->set(key, cache)
      await memento->set(legacyKey, Dict.make())
    }

    let entries = (memento: t): Dict.t<entry> =>
      {
        let current = readEntries(memento, key)
        if current->Dict.toArray->Array.length > 0 {
          current
        } else {
          readEntries(memento, legacyKey)
        }
      }

    let get = (memento: t, resolved: Resolved.t): option<entry> => {
      let cache = entries(memento)
      cache->Dict.get(Resolved.toString(resolved))
    }

    let setKind = async (
      memento: t,
      resolved: Resolved.t,
      kind: kind,
    ): unit => {
      let cache = entries(memento)
      let entry: entry = {kind, timestamp: Date.make(), error: None}
      cache->Dict.set(Resolved.toString(resolved), entry)
      await writeEntries(memento, cache)
    }

    let setError = async (memento: t, resolved: Resolved.t, error: string): unit => {
      let cache = entries(memento)
      let resolvedKey = Resolved.toString(resolved)
      let existingKind = switch cache->Dict.get(resolvedKey) {
      | Some(existingEntry) => existingEntry.kind
      | None => Unknown
      }
      let entry: entry = {
        kind: existingKind,
        timestamp: Date.make(),
        error: Some(error),
      }
      cache->Dict.set(resolvedKey, entry)
      await writeEntries(memento, cache)
    }

    let clear = async (memento: t): unit => {
      await writeEntries(memento, Dict.make())
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
