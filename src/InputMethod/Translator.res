// key - symbols mapping
@bs.module("./../../../../asset/query.js")
external rawTable: Js.Dict.t<array<string>> = "default"

// trie

type rec trie = {
  symbols: array<string>,
  subTrie: Js.Dict.t<trie>,
}
@bs.module("./../../../../asset/keymap.js")
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
    ->Array.map(key => (key, fromObject(%raw(`
      obj[key]
    `))))
    ->Js.Dict.fromArray
  {symbols: symbols, subTrie: subTrie}
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

let decode: Json.Decode.decoder<translation> = json => {
  open Json.Decode
  {
    symbol: json |> field("symbol", optional(string)),
    further: json |> field("further", bool),
    keySuggestions: json |> field("keySuggestions", array(string)),
    candidateSymbols: json |> field("candidateSymbols", array(string)),
  }
}

let encode: Json.Encode.encoder<translation> = translation => {
  open Json.Encode
  object_(list{
    ("symbol", translation.symbol |> nullable(string)),
    ("further", translation.further |> bool),
    ("keySuggestions", translation.keySuggestions |> array(string)),
    ("candidateSymbols", translation.candidateSymbols |> array(string)),
  })
}

/* converts characters to symbol, and tells if there's any further possible combinations */
let translate = (input: string): translation => {
  let trie = isInKeymap(input)
  let keySuggestions = trie->Option.mapWithDefault([], toKeySuggestions)
  // ->Extension.extendKeySuggestions(input);

  let candidateSymbols = trie->Option.mapWithDefault([], toCandidateSymbols)

  {
    symbol: candidateSymbols[0],
    further: Array.length(keySuggestions) != 0,
    keySuggestions: keySuggestions,
    candidateSymbols: candidateSymbols,
  }
}
let initialTranslation = translate("")

let lookup = (symbol): option<array<string>> =>
  Js.String.codePointAt(0, symbol)->Option.map(string_of_int)->Option.flatMap(Js.Dict.get(rawTable))
