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
      // Js.log("[ add ][ PendingInit ]" ++ fileName)
      // Fulfill the request for Semantic Tokens
      state.highlighting->Highlighting.getSemanticTokens->resolve
      // set the entry as Initialized
      dict->Js.Dict.set(fileName, Initialized(state))
    | Some(Initialized(_)) => // do nothing
      // Js.log("[ add ][ Initialized ]" ++ fileName)
      ()
    | None =>
      // Js.log("[ add ][ None ]" ++ fileName)
      dict->Js.Dict.set(fileName, Initialized(state))
    }

  // Removes the entry (but without triggering State.destroy() )
  let remove = fileName => Util.Dict.delete(dict, fileName)

  let removeAndDestroy = async fileName =>
    switch get'(fileName) {
    | None => ()
    | Some(PendingInit(_promise, resolve)) =>
      remove(fileName)
      resolve([])
    | Some(Initialized(state)) =>
      remove(fileName)
      State.destroy(state, false)->ignore
      ()
    }

  let removeAndDestroyAll = async () => {
    let _ = await dict->Js.Dict.keys->Array.map(removeAndDestroy)->Util.oneByOne
  }

  let isEmpty = () => Js.Dict.keys(dict)->Array.length == 0

  // Requesting Semantic Tokens
  // add PendingInit(_) to the Registry if the entry has not been created yet
  let requestSemanticTokens = async fileName =>
    switch get'(fileName) {
    | Some(PendingInit(promise, _resolve)) => // Js.log("[ req ][ PendingInit ]" ++ fileName)
      await promise
    | Some(Initialized(state)) =>
      // Js.log("[ req ][ Initialized ]" ++ fileName)
      state.highlighting->Highlighting.getSemanticTokens
    | None =>
      // Js.log("[ req ][ None ]" ++ fileName)
      let (promise, resolve, _) = Util.Promise_.pending()
      dict->Js.Dict.set(fileName, PendingInit(promise, resolve))
      await promise
    }
}

include Module
