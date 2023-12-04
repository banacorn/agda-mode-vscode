// key - symbols mapping
@module("./../../../../asset/query.js")
external rawTable: Js.Dict.t<array<string>> = "default"

// trie

type rec trie = {
  symbols: array<string>,
  subTrie: Js.Dict.t<trie>,
}
@module("./../../../../asset/keymap.js")
external rawKeymapObject: {.} = "default"

open Belt

let rec fromObject = (obj: {.}): trie => {
  let symbols = %raw(`
    obj[">>"] || []
  `)
  let subTrie =
    obj
    ->Js.Obj.keys
    ->Array.keep(key => key != ">>")
    ->Array.map(key => (
      key,
      fromObject(
        %raw(`
      obj[key]
    `),
      ),
    ))
    ->Js.Dict.fromArray
  {symbols, subTrie}
}

let keymap = fromObject(rawKeymapObject)

let toKeySuggestions = (trie: trie): array<string> => Js.Dict.keys(trie.subTrie)

let toCandidateSymbols = (trie: trie): array<string> => trie.symbols

// see if the key sequence is in the keymap
// returns (KeySuggestions, CandidateSymbols)
let isInKeymap = (input: string): option<trie> => {
  let rec helper = (input: string, trie: trie): option<trie> =>
    switch String.length(input) {
    | 0 => Some(trie)
    | n =>
      let key = Js.String.substrAtMost(~from=0, ~length=1, input)
      let rest = Js.String.substrAtMost(~from=1, ~length=n - 1, input)
      switch Js.Dict.get(trie.subTrie, key) {
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
  let keySuggestions = trie->Option.mapWithDefault([], toKeySuggestions)
  // ->Extension.extendKeySuggestions(input);
  let further = Array.length(keySuggestions) != 0
  let candidateSymbols = trie->Option.mapWithDefault([], toCandidateSymbols)

  let symbol = candidateSymbols[0]
  let last = Js.String.sliceToEnd(~from=-1, input)->Belt.Int.fromString
  open Belt.Option

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
let initialTranslation = translate("")

let lookup = (symbol): option<array<string>> =>
  Js.String.codePointAt(0, symbol)->Option.map(string_of_int)->Option.flatMap(Js.Dict.get(rawTable))

let decode = {
  open JsonCombinators.Json.Decode
  // TODO: replace `field.required(. "symbol", option(string))` with `field.optional(. "symbol", string)`
  object(field => {
    symbol: field.required(. "symbol", option(string)),
    further: field.required(. "further", bool),
    keySuggestions: field.required(. "keySuggestions", array(string)),
    candidateSymbols: field.required(. "candidateSymbols", array(string)),
  })
}

let encode = translation => {
  open JsonCombinators.Json.Encode
  Unsafe.object({
    "symbol": option(string, translation.symbol),
    "further": bool(translation.further),
    "keySuggestions": array(string, translation.keySuggestions),
    "candidateSymbols": array(string, translation.candidateSymbols),
  })
}
