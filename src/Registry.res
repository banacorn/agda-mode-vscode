module Module: {
  module Entry: {
    type t = {
      mutable state: option<State.t>,
      semanticTokens: Resource.t<array<Highlighting.SemanticToken.t>>,
    }
  }

  let getState: string => option<State.t>
  let getEntry: string => option<Entry.t>
  let getAllStates: unit => array<State.t>
  let add: (string, State.t) => unit
  let remove: string => unit
  let removeAndDestroy: string => promise<unit>
  let removeAndDestroyAll: unit => promise<unit>
  let isEmpty: unit => bool

  let requestSemanticTokens: string => promise<array<Highlighting.SemanticToken.t>>
} = {
  module Entry = {
    type tokens = array<Highlighting.SemanticToken.t>

    type t = {
      mutable state: option<State.t>,
      semanticTokens: Resource.t<tokens>,
    }

    // create a new Entry and also return with a pending promise for SemanticTokens
    let make = state =>
      switch state {
      | None => {state: None, semanticTokens: Resource.make()}
      | Some(state: State.t) =>
        let semanticTokens = state.tokens->Tokens.getVSCodeTokens
        {
          state: Some(state),
          semanticTokens,
        }
      }
  }

  // FileName-Entry Registry
  let dict: Dict.t<Entry.t> = Dict.make()

  let getEntry = fileName => dict->Dict.get(fileName)
  let getState = fileName => getEntry(fileName)->Option.flatMap(x => x.state)

  //  Get all existing States
  let getAllStates = () => dict->Dict.valuesToArray->Array.filterMap(getEntry => getEntry.state)

  // Adds an instantiated State to the Registry
  let add = (fileName, state: State.t) =>
    switch getEntry(fileName) {
    | None =>
      // entry not found, create a new one
      dict->Dict.set(fileName, Entry.make(Some(state)))
    | Some(entry) =>
      switch entry.state {
      | Some(state) => entry.state = Some(state) // update the state
      | None => dict->Dict.set(fileName, Entry.make(Some(state)))
      }
    }

  // Removes the entry (but without triggering State.destroy() )
  let remove = fileName => Dict.delete(dict, fileName)

  let removeAndDestroy = async fileName =>
    switch getEntry(fileName) {
    | None => ()
    | Some(entry) =>
      remove(fileName)
      switch entry.state {
      | None => ()
      | Some(state) =>
        let _ = await State.destroy(state, false)
      }
    }

  let removeAndDestroyAll = async () => {
    let _ =
      await dict
      ->Dict.keysToArray
      ->Array.map(pair => () => removeAndDestroy(pair))
      ->Util.Promise_.oneByOne
  }

  let isEmpty = () => Dict.keysToArray(dict)->Array.length == 0

  // Requesting Semantic Tokens
  let requestSemanticTokens = async fileName => {
    switch getEntry(fileName) {
    | Some(entry) =>
      let tokens = await entry.semanticTokens->Resource.get
      tokens
    | None =>
      // entry not found, create a new one
      let entry = Entry.make(None)
      dict->Dict.set(fileName, entry)
      let tokens = await entry.semanticTokens->Resource.get
      tokens
    }
  }

  // let provideSemanticTokens = tokens =>
  //   switch getEntry(tokens.fileName) {
  //   | Some(entry) =>
  //     entry.semanticTokens->Option.forEach(semanticTokens => semanticTokens(tokens))
  //     entry.state->Option.forEach(state => State.provideSemanticTokens(state, tokens))
  //   | None =>
  //     // entry not found, create a new one
  //     let (entry, _) = Entry.make(Some(tokens.state))
  //     dict->Dict.set(tokens.fileName, entry)
  //     entry.semanticTokens->Option.forEach(semanticTokens => semanticTokens(tokens))
  //   }
}

include Module
