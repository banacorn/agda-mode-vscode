module Module: {
  let get: string => option<State.t>
  let getAll: unit => array<State.t>
  let add: (string, State.t) => unit
  let remove: string => unit
  let removeAndDestroy: string => promise<unit>
  let removeAndDestroyAll: unit => promise<unit>
  let isEmpty: unit => bool

  let requestSemanticTokens: string => promise<array<Highlighting.SemanticToken.t>>
} = {
  module Status = {
    type tokens = array<Highlighting.SemanticToken.t>

    type t =
      // Waiting for Semantic Tokens, but the State has not been initiated yet
      | PendingInit(promise<tokens>, tokens => unit)
      // State has been initiated yet
      | Initialized(State.t)

    let getState = status =>
      switch status {
      | PendingInit(_promise, _resolve) => None
      | Initialized(state) => Some(state)
      }
  }

  // A dictionary of FileName-Status entries
  let dict: Dict.t<Status.t> = Dict.make()

  // Private helper that returns Status
  let get' = fileName => dict->Dict.get(fileName)
  // Public getter that returns State
  let get = fileName => get'(fileName)->Option.flatMap(Status.getState)

  //  Get all existing States
  let getAll = () => dict->Dict.valuesToArray->Array.filterMap(Status.getState)

  // Adds an instantiated State to the Registry
  let add = (fileName, state: State.t) =>
    switch get'(fileName) {
    | Some(PendingInit(_, resolve)) =>
      // Fulfill the request for Semantic Tokens
      state.highlighting->Highlighting.getSemanticTokens->resolve
      // set the entry as Initialized
      dict->Dict.set(fileName, Initialized(state))
    | Some(Initialized(_)) => // do nothing
      ()
    | None => dict->Dict.set(fileName, Initialized(state))
    }

  // Removes the entry (but without triggering State.destroy() )
  let remove = fileName => Dict.delete(dict, fileName)

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
    let _ = await dict->Dict.keysToArray->Array.map(removeAndDestroy)->Util.oneByOne
  }

  let isEmpty = () => Dict.keysToArray(dict)->Array.length == 0

  // Requesting Semantic Tokens
  // add PendingInit(_) to the Registry if the entry has not been created yet
  let requestSemanticTokens = async fileName =>
    switch get'(fileName) {
    | Some(PendingInit(promise, _resolve)) => await promise
    | Some(Initialized(state)) => state.highlighting->Highlighting.getSemanticTokens
    | None =>
      let (promise, resolve, _) = Util.Promise_.pending()
      dict->Dict.set(fileName, PendingInit(promise, resolve))
      await promise
    }
}

include Module
