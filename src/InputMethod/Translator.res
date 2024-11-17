// key - symbols mapping
@module("./../../../../asset/query.js")
external rawTable: Dict.t<array<string>> = "default"

// trie

type rec trie = {
  symbols: array<string>,
  subTrie: Dict.t<trie>,
}
@module("./../../../../asset/keymap.js")
external rawKeymapObject: {.} = "default"

let rec fromObject = (obj: {.}): trie => {
  let symbols = %raw(`
    obj[">>"] || []
  `)
  let subTrie =
    obj
    ->Object.keysToArray
    ->Array.filter(key => key != ">>")
    ->Array.map(key => (
      key,
      fromObject(
        %raw(`
      obj[key]
    `),
      ),
    ))
    ->Dict.fromArray
  {symbols, subTrie}
}

let keymap = fromObject(rawKeymapObject)

let toKeySuggestions = (trie: trie): array<string> => Dict.keysToArray(trie.subTrie)

let toCandidateSymbols = (trie: trie): array<string> => trie.symbols

// see if the key sequence is in the keymap
// returns (KeySuggestions, CandidateSymbols)
let isInKeymap = (input: string): option<trie> => {
  let rec helper = (input: string, trie: trie): option<trie> =>
    switch String.length(input) {
    | 0 => Some(trie)
    | n =>
      let key = input->String.substring(~start=0, ~end=1)
      let rest = input->String.substring(~start=1, ~end=n)
      switch Dict.get(trie.subTrie, key) {
      | Some(trie') => helper(rest, trie')
      | None => None
      }
    }
  helper(input, keymap)
}

// // see if the key sequence is in the extension */
// let isInExtension = (input: string): option(trie) => {};

type translation = {
  symbol: option<string>,
  further: bool,
  keySuggestions: array<string>,
  candidateSymbols: array<string>,
}

type state = {
  lastTranslation: translation,
  candidateIndex: int,
}

/* converts characters to symbol, and tells if there's any further possible combinations */
let translate = (input: string, state: option<state>): translation => {
  let trie = isInKeymap(input)
  let keySuggestions = trie->Option.mapOr([], toKeySuggestions)
  // ->Extension.extendKeySuggestions(input);
  let further = Array.length(keySuggestions) != 0
  let candidateSymbols = trie->Option.mapOr([], toCandidateSymbols)

  let symbol = candidateSymbols[0]
  let last = input->String.sliceToEnd(~start=-1)->Int.fromString
  open Option

  // If user inputs a number and the new sequence can not be translated into symbols,
  // this number may be the index of candidateSymbols
  if (
    isSome(last) &&
    symbol == None &&
    isSome(state) &&
    Array.length(getExn(state).lastTranslation.candidateSymbols) != 0
  ) {
    let state = getExn(state)
    let cycle_Zplus = n =>
      if n == 0 {
        9
      } else {
        n - 1
      }
    let index = cycle_Zplus(getExn(last)) + state.candidateIndex / 10 * 10
    {
      symbol: state.lastTranslation.candidateSymbols[
        min(index, Array.length(state.lastTranslation.candidateSymbols) - 1)
      ],
      further,
      keySuggestions,
      candidateSymbols,
    }
  } else {
    {
      symbol,
      further,
      keySuggestions,
      candidateSymbols,
    }
  }
}
let initialTranslation = x => translate("", x)

let lookup = (symbol): option<array<string>> =>
  symbol->String.codePointAt(0)->Option.map(string_of_int)->Option.flatMap(Dict.get(rawTable, ...))

let decode = {
  open JsonCombinators.Json.Decode
  // TODO: replace `field.required(. "symbol", option(string))` with `field.optional(. "symbol", string)`
  object(field => {
    symbol: field.required("symbol", option(string)),
    further: field.required("further", bool),
    keySuggestions: field.required("keySuggestions", array(string)),
    candidateSymbols: field.required("candidateSymbols", array(string)),
  })
}

let encode = translation => {
  open JsonCombinators.Json.Encode
  Unsafe.object({
    "symbol": option(string)(translation.symbol),
    "further": bool(translation.further),
    "keySuggestions": array(string)(translation.keySuggestions),
    "candidateSymbols": array(string)(translation.candidateSymbols),
  })
}
