module Module: {
  module Entry: {
    type t = {
      mutable state: option<State.t>,
      semanticTokens: Resource.t<array<Highlighting__SemanticToken.t>>,
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

  let requestSemanticTokens: string => promise<array<Highlighting__SemanticToken.t>>
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

  let getEntry = fileName => {
    Js.Console.log("Registry.getEntry: Looking up entry for " ++ fileName)
    let result = dict->Dict.get(fileName)
    switch result {
    | Some(_) => Js.Console.log("Registry.getEntry: Found entry for " ++ fileName)
    | None =>
      Js.Console.log("Registry.getEntry: No entry found for " ++ fileName)
      // Log all existing keys for debugging
      let allKeys = dict->Dict.keysToArray
      Js.Console.log("Registry.getEntry: All existing keys: " ++ allKeys->Array.join(", "))
    }
    result
  }
  let getState = fileName => getEntry(fileName)->Option.flatMap(x => x.state)

  //  Get all existing States
  let getAllStates = () => dict->Dict.valuesToArray->Array.filterMap(getEntry => getEntry.state)

  // Adds an instantiated State to the Registry
  let add = (fileName, state: State.t) => {
    Js.Console.log("Registry.add: Adding entry for " ++ fileName)
    
    switch getEntry(fileName) {
    | None =>
      // entry not found, create a new one
      Js.Console.log("Registry.add: Creating new entry for " ++ fileName)
      let entry = Entry.make(Some(state))
      dict->Dict.set(fileName, entry)
    | Some(entry) =>
      Js.Console.log("Registry.add: Updating existing entry for " ++ fileName)
      switch entry.state {
      | Some(state) => entry.state = Some(state) // update the state
      | None => 
        let newEntry = Entry.make(Some(state))
        dict->Dict.set(fileName, newEntry)
      }
    }
  }

  // Removes the entry (but without triggering State.destroy() )
  let remove = fileName => Dict.delete(dict, fileName)

  let removeAndDestroy = async fileName => {
    Js.Console.log("Registry.removeAndDestroy: Starting for " ++ fileName)
    switch getEntry(fileName) {
    | None => Js.Console.log("Registry.removeAndDestroy: No entry found for " ++ fileName)
    | Some(entry) =>
      Js.Console.log("Registry.removeAndDestroy: Found entry for " ++ fileName)
      remove(fileName)
      Js.Console.log("Registry.removeAndDestroy: Removed entry for " ++ fileName)
      switch entry.state {
      | None => Js.Console.log("Registry.removeAndDestroy: No state to destroy for " ++ fileName)
      | Some(state) =>
        Js.Console.log("Registry.removeAndDestroy: Destroying state for " ++ fileName)
        let _ = await State.destroy(state, false)
        Js.Console.log("Registry.removeAndDestroy: Completed state destruction for " ++ fileName)
      }
    }
    Js.Console.log("Registry.removeAndDestroy: Completed for " ++ fileName)
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
    Js.Console.log("Registry.requestSemanticTokens: Starting request for " ++ fileName)
    switch getEntry(fileName) {
    | Some(entry) =>
      Js.Console.log("Registry.requestSemanticTokens: Found existing entry for " ++ fileName)
      let tokens = await entry.semanticTokens->Resource.get
      Js.Console.log(
        "Registry.requestSemanticTokens: Got " ++
        string_of_int(Array.length(tokens)) ++ " tokens from existing entry",
      )
      tokens
    | None =>
      // entry not found, create a new one
      Js.Console.log(
        "Registry.requestSemanticTokens: No entry found, creating new one for " ++ fileName,
      )
      let entry = Entry.make(None)
      dict->Dict.set(fileName, entry)
      let tokens = await entry.semanticTokens->Resource.get
      Js.Console.log(
        "Registry.requestSemanticTokens: Got " ++
        string_of_int(Array.length(tokens)) ++ " tokens from new entry",
      )
      tokens
    }
  }
}

include Module
