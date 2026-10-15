open Mocha
open Test__Util

// Agda language IDs (`agda` and `lagda-*`) listed in `contributes.languages` of package.json
let readAgdaLanguageIds = async (): array<string> => {
  let packageJsonContent = await File.read(Path.toAbsolute("../../../../package.json"))
  packageJsonContent
  ->Js.Json.parseExn
  ->Js.Json.decodeObject
  ->Option.flatMap(obj => obj->Js.Dict.get("contributes"))
  ->Option.flatMap(contributes => contributes->Js.Json.decodeObject)
  ->Option.flatMap(obj => obj->Js.Dict.get("languages"))
  ->Option.flatMap(languages => languages->Js.Json.decodeArray)
  ->Option.getOr([])
  ->Array.filterMap(language =>
    language
    ->Js.Json.decodeObject
    ->Option.flatMap(obj => obj->Js.Dict.get("id"))
    ->Option.flatMap(id => id->Js.Json.decodeString)
  )
  ->Array.filter(id => id == "agda" || id->String.startsWith("lagda-"))
}

let sorted = xs => xs->Array.toSorted(String.compare)

describe("Language selectors", () => {
  Async.it(
    "should select exactly the Agda language IDs in contributes.languages for providers",
    async () => {
      let expected = await readAgdaLanguageIds()

      let actual = Editor.Provider.documentSelector->Array.filterMap(selector =>
        switch VSCode.StringOr.classify(selector) {
        | String(language) => Some(language)
        | Others(_) => None
        }
      )

      Assert.deepStrictEqual(sorted(actual), sorted(expected))
    },
  )

  Async.it(
    "should select exactly the Agda language IDs in contributes.languages for the language client",
    async () => {
      let expected = await readAgdaLanguageIds()

      let actual =
        Connection__Protocol__LSP.documentSelector("agda")->Array.filterMap(selector =>
          switch VSCode.StringOr.classify(selector) {
          | String(language) => Some(language)
          | Others(filter) => filter.language
          }
        )

      Assert.deepStrictEqual(sorted(actual), sorted(expected))
    },
  )
})
