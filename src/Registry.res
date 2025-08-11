module Module: {
  module Entry: {
    type t = {
      mutable state: option<State.t>,
      semanticTokens: Resource.t<array<Highlighting__SemanticToken.t>>,
    }
  }

  let get: VSCode.TextDocument.t => option<Entry.t>
  let getAll: unit => array<Entry.t>
  let add: (VSCode.TextDocument.t, State.t) => unit
  let remove: VSCode.TextDocument.t => unit
  let removeAndDestroy: VSCode.TextDocument.t => promise<unit>
  let removeAndDestroyAll: unit => promise<unit>
  let isEmpty: unit => bool

  let requestSemanticTokens: VSCode.TextDocument.t => promise<array<Highlighting__SemanticToken.t>>
} = {
  module Entry = {
    type tokens = array<Highlighting__SemanticToken.t>

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

  let get = document => dict->Dict.get(document->VSCode.TextDocument.fileName)
  let getAll = () => dict->Dict.valuesToArray

  // Adds an instantiated State to the Registry
  let add = (document, state: State.t) => {
    switch get(document) {
    | None =>
      // entry not found, create a new one
      let entry = Entry.make(Some(state))
      dict->Dict.set(document->VSCode.TextDocument.fileName, entry)
    | Some(entry) =>
      switch entry.state {
      | Some(state) => entry.state = Some(state) // update the state
      | None =>
        let newEntry = Entry.make(Some(state))
        dict->Dict.set(document->VSCode.TextDocument.fileName, newEntry)
      }
    }
  }

  // Removes the entry (but without triggering State.destroy() )
  let remove = document => Dict.delete(dict, document->VSCode.TextDocument.fileName)

  let removeAndDestroyPrim = async filepath => {
    switch Dict.get(dict, filepath) {
    | None => ()
    | Some(entry) =>
      Dict.delete(dict, filepath)
      switch entry.state {
      | None => ()
      | Some(state) =>
        let _ = await State.destroy(state, false)
      }
    }
  }

  let removeAndDestroy = document => removeAndDestroyPrim(document->VSCode.TextDocument.fileName)

  let removeAndDestroyAll = async () => {
    let _ =
      await dict
      ->Dict.keysToArray
      ->Array.map(filepath => () => removeAndDestroyPrim(filepath))
      ->Util.Promise_.oneByOne
  }

  let isEmpty = () => Dict.keysToArray(dict)->Array.length == 0

  // Requesting Semantic Tokens
  let requestSemanticTokens = async document => {
    switch get(document) {
    | Some(entry) =>
      let tokens = await entry.semanticTokens->Resource.get
      tokens
    | None =>
      // entry not found, create a new one
      let entry = Entry.make(None)
      dict->Dict.set(document->VSCode.TextDocument.fileName, entry)
      let tokens = await entry.semanticTokens->Resource.get
      tokens
    }
  }
}

include Module
