open Mocha
open Test__Util

// Unique paths of the grammars listed in `contributes.grammars` of package.json
let readGrammarPaths = async (): array<string> => {
  let packageJsonContent = await File.read(Path.toAbsolute("../../../../package.json"))
  let paths =
    packageJsonContent
    ->Js.Json.parseExn
    ->Js.Json.decodeObject
    ->Option.flatMap(obj => obj->Js.Dict.get("contributes"))
    ->Option.flatMap(contributes => contributes->Js.Json.decodeObject)
    ->Option.flatMap(obj => obj->Js.Dict.get("grammars"))
    ->Option.flatMap(grammars => grammars->Js.Json.decodeArray)
    ->Option.getOr([])
    ->Array.filterMap(grammar =>
      grammar
      ->Js.Json.decodeObject
      ->Option.flatMap(obj => obj->Js.Dict.get("path"))
      ->Option.flatMap(path => path->Js.Json.decodeString)
    )
  paths->Array.filterWithIndex((path, i) => paths->Array.indexOf(path) == i)
}

describe("Grammars", () => {
  Async.it("should parse every grammar in contributes.grammars as JSON", async () => {
    let paths = await readGrammarPaths()

    let unparsable = []
    for i in 0 to Array.length(paths) - 1 {
      let path = paths->Array.getUnsafe(i)
      let content = await File.read(Path.toAbsolute(NodeJs.Path.join(["../../../../", path])))
      switch JSON.parseExn(content) {
      | _ => ()
      | exception _ => unparsable->Array.push(path)
      }
    }

    Assert.deepStrictEqual(unparsable, [])
  })
})
