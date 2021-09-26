module Module: {
  let get: string => option<State.t>
  let add: (string, State.t) => unit
  let remove: string => unit
  let removeAndDestroy: string => Promise.t<unit>
  let removeAndDestroyAll: unit => Promise.t<unit>
  let isEmpty: unit => bool

  let requestSemanticTokens: string => Promise.t<array<Highlighting.SemanticToken.t>>
} = {
  open Belt

  module Status = {
    type tokens = array<Highlighting.SemanticToken.t>

    type t =
      // Waiting for Semantic Tokens, but the State has not been initiated yet
      | PendingInit(Promise.t<tokens>, tokens => unit)
      // State has been initiated yet
      | Initialized(State.t)

    let getState = status =>
      switch status {
      | PendingInit(_promise, _resolve) => None
      | Initialized(state) => Some(state)
      }
  }

  // A dictionary of FileName-Status entries
  let dict: Js.Dict.t<Status.t> = Js.Dict.empty()

  // Private helper that returns Status
  let get' = fileName => dict->Js.Dict.get(fileName)
  // Public getter that returns State
  let get = fileName => get'(fileName)->Option.flatMap(Status.getState)

  // Adds an instantiated State to the Registry
  let add = (fileName, state: State.t) =>
    switch get'(fileName) {
    | Some(PendingInit(_, resolve)) =>
      // Fulfill the request for Semantic Tokens
      state.highlighting->Highlighting.requestSemanticTokens->Promise.get(resolve)
      // set the entry as Initialized
      dict->Js.Dict.set(fileName, Initialized(state))
    | Some(Initialized(_)) => () // do nothing
    | None => dict->Js.Dict.set(fileName, Initialized(state))
    }

  // Removes the entry (but without triggering State.destroy() )
  let remove = fileName => {
    let delete_: (Js.Dict.t<'a>, string) => unit = %raw("function (dict, key) {delete dict[key]}")
    delete_(dict, fileName)
  }

  let removeAndDestroy = fileName =>
    switch get'(fileName) {
    | None => Promise.resolved()
    | Some(PendingInit(_promise, resolve)) =>
      remove(fileName)
      resolve([])
      Promise.resolved()
    | Some(Initialized(state)) =>
      remove(fileName)
      State.destroy(state, false)
    }

  let removeAndDestroyAll = () => {
    dict->Js.Dict.keys->Array.map(removeAndDestroy)->Util.oneByOne->Promise.map(_ => ())
  }

  let isEmpty = () => Js.Dict.keys(dict)->Array.length == 0

  // Requesting Semantic Tokens
  // add PendingInit(_) to the Registry if the entry has not been created yet
  let requestSemanticTokens = fileName =>
    switch get'(fileName) {
    | Some(PendingInit(promise, _resolve)) => promise
    | Some(Initialized(state)) => state.highlighting->Highlighting.requestSemanticTokens
    | None =>
      let (promise, resolve) = Promise.pending()
      dict->Js.Dict.set(fileName, PendingInit(promise, resolve))
      promise
    }
}

include Module
