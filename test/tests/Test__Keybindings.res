open Mocha
open Test__Util

// Keybindings listed in `contributes.keybindings` of package.json, as JSON objects
let readKeybindings = async (): array<Js.Dict.t<Js.Json.t>> => {
  let packageJsonContent = await File.read(Path.toAbsolute("../../../../package.json"))
  packageJsonContent
  ->Js.Json.parseExn
  ->Js.Json.decodeObject
  ->Option.flatMap(obj => obj->Js.Dict.get("contributes"))
  ->Option.flatMap(contributes => contributes->Js.Json.decodeObject)
  ->Option.flatMap(obj => obj->Js.Dict.get("keybindings"))
  ->Option.flatMap(keybindings => keybindings->Js.Json.decodeArray)
  ->Option.getOr([])
  ->Array.filterMap(keybinding => keybinding->Js.Json.decodeObject)
}

let getString = (obj, field) => obj->Js.Dict.get(field)->Option.flatMap(value => value->Js.Json.decodeString)

describe("Keybindings", () => {
  // Issue #47: VS Code does not fall back to the single-key command when a chord is abandoned,
  // so any chord starting with bare `ctrl+x` swallows Cut
  Async.it("should not start any keybinding with bare ctrl+x", async () => {
    let keybindings = await readKeybindings()

    let startingWithCtrlX = keybindings->Array.flatMap(keybinding => {
      let command = keybinding->getString("command")->Option.getOr("")
      ["key", "mac"]->Array.filterMap(field =>
        switch keybinding->getString(field) {
        | Some(key) if key->String.startsWith("ctrl+x ") => Some(command ++ " (" ++ field ++ "): " ++ key)
        | _ => None
        }
      )
    })

    Assert.deepStrictEqual(startingWithCtrlX, [])
  })
})
