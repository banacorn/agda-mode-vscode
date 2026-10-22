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

  // Issue #69: agda-mode refuses the input method on a Live Share guest's document,
  // so a backslash bound to it there would be swallowed instead of typed
  Async.it("should not bind backslash on vsls: documents", async () => {
    let keybindings = await readKeybindings()

    let backslashOnGuest = keybindings->Array.filterMap(keybinding =>
      switch (keybinding->getString("key"), keybinding->getString("when")) {
      | (Some("[Backslash]"), when_) =>
        let when_ = when_->Option.getOr("")
        when_->String.includes("resourceScheme != vsls") ? None : Some(when_)
      | _ => None
      }
    )

    Assert.deepStrictEqual(backslashOnGuest, [])
  })

  // Issue #76: a backslash typed while the input method is on must reach it,
  // to commit the current symbol and start the next one
  Async.it("should bind backslash while the input method is on, keeping its other guards", async () => {
    let keybindings = await readKeybindings()

    let backslashGuards = keybindings->Array.filterMap(keybinding =>
      switch (
        keybinding->getString("command"),
        keybinding->getString("key"),
        keybinding->getString("when"),
      ) {
      | (Some("agda-mode.input-symbol[Activate]"), Some("[Backslash]"), when_) =>
        Some(when_->Option.getOr("")->String.split(" && "))
      | _ => None
      }
    )

    Assert.deepStrictEqual(
      backslashGuards,
      [
        [
          "config.agdaMode.inputMethod.enabled",
          "editorLangId =~ /.*(l?agda(-(markdown|typst|latex|tex|rst|org|forester))?).*/",
          "editorTextFocus",
          "resourceScheme != vsls",
        ],
      ],
    )
  })

  // Issue #78: `&&` binds tighter than `||` in a `when` clause,
  // so agdaModeTyping alone must not bind escape outside an Agda editor
  Async.it("should bind escape only in an Agda editor, while prompting or typing", async () => {
    let keybindings = await readKeybindings()

    let escapeWhens = keybindings->Array.filterMap(keybinding =>
      switch keybinding->getString("command") {
      | Some("agda-mode.escape") => keybinding->getString("when")
      | _ => None
      }
    )

    Assert.deepStrictEqual(
      escapeWhens,
      [
        "editorLangId =~ /.*(l?agda(-(markdown|typst|latex|tex|rst|org|forester))?).*/ && (agdaModePrompting || agdaModeTyping)",
      ],
    )
  })
})
