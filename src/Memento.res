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
  // primitive operations
  let get: (t, string) => option<'a>
  let getWithDefault: (t, string, 'a) => 'a
  let keys: t => array<string>
  let set: (t, string, 'a) => promise<unit>
  // for debugging
  let toString: t => string

  module EndpointVersion: {
    type entry = {
      version: option<string>,
      timestamp: Js.Date.t,
      error: option<string>,
    }
    let get: (t, string) => option<entry>
    let setVersion: (t, string, string) => promise<unit>
    let setError: (t, string, string) => promise<unit>
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

  let keys = context =>
    switch context {
    | Memento(context) => VSCode.Memento.keys(context)
    | Mock(dict) => dict->Dict.keysToArray
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

  module EndpointVersion = {
    type entry = {
      version: option<string>,
      timestamp: Js.Date.t,
      error: option<string>,
    }

    let key = "endpointVersion"

    let get = (memento: t, path: string): option<entry> => {
      let cache = memento->getWithDefault(key, Dict.make())
      cache->Dict.get(path)
    }

    let setVersion = async (memento: t, path: string, version: string): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let entry = {version: Some(version), timestamp: Js.Date.make(), error: None}
      cache->Dict.set(path, entry)
      await memento->set(key, cache)
    }

    let setError = async (memento: t, path: string, error: string): unit => {
      let cache = memento->getWithDefault(key, Dict.make())
      let entry = {version: None, timestamp: Js.Date.make(), error: Some(error)}
      cache->Dict.set(path, entry)
      await memento->set(key, cache)
    }
  }
}

include Module
