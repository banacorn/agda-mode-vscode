open Mocha
open Test__Util

// These commands do not have keybindings
let noKeybinding = [Command.OpenDebugBuffer, SwitchAgdaVersion2]

// These commands are not actual Agda commands, but are used for input method or other purposes
let pseudoCommand = [
  Command.InputMethod(BrowseUp),
  InputMethod(BrowseDown),
  InputMethod(BrowseLeft),
  InputMethod(BrowseRight),
  InputMethod(InsertChar("{")),
  InputMethod(InsertChar("(")),
]

module PackageJson = {
  type t = {
    commands: array<string>,
    keybindings: array<string>,
  }

  let read = async (): t => {
    let packageJsonPath = Path.toAbsolute("../../../../package.json")
    let packageJsonContent = await File.read(packageJsonPath)
    let packageJson = Js.Json.parseExn(packageJsonContent)

    let extractCommands = (section: string): array<string> => {
      packageJson
      ->Js.Json.decodeObject
      ->Option.flatMap(obj => obj->Js.Dict.get("contributes"))
      ->Option.flatMap(contributes => contributes->Js.Json.decodeObject)
      ->Option.flatMap(obj => obj->Js.Dict.get(section))
      ->Option.flatMap(items => items->Js.Json.decodeArray)
      ->Option.getOr([])
      ->Array.filterMap(item => {
        item
        ->Js.Json.decodeObject
        ->Option.flatMap(obj => obj->Js.Dict.get("command"))
        ->Option.flatMap(command => command->Js.Json.decodeString)
      })
      ->Array.map(cmd => {
        // Remove "agda-mode." prefix if present
        if String.startsWith(cmd, "agda-mode.") {
          String.sliceToEnd(cmd, ~start=10)
        } else {
          cmd
        }
      })
    }

    {
      commands: extractCommands("commands"),
      keybindings: extractCommands("keybindings"),
    }
  }
}

let shouldBeRegisteredAsCommand = (command: Command.t): bool => {
  !(pseudoCommand->Array.some(pseudo => pseudo == command))
}

let shouldHaveKeybinding = (command: Command.t): bool => {
  shouldBeRegisteredAsCommand(command) && 
  !(noKeybinding->Array.some(noKb => noKb == command))
}

describe("Command Registration", () => {
  Async.it("should have all valid commands registered in package.json", async () => {
    let packageJson = await PackageJson.read()

    Command.names
    ->Array.filter(((command, _)) => shouldBeRegisteredAsCommand(command))
    ->Array.forEach(((_, commandName)) => {
      let found = packageJson.commands->Array.some(name => name == commandName)
      if !found {
        Assert.fail(`Command "${commandName}" is not registered in package.json commands`)
      }
    })
  })

  Async.it("should have all valid commands with keybindings registered", async () => {
    let packageJson = await PackageJson.read()

    Command.names
    ->Array.filter(((command, _)) => shouldHaveKeybinding(command))
    ->Array.forEach(((_, commandName)) => {
      let found = packageJson.keybindings->Array.some(name => name == commandName)
      if !found {
        Assert.fail(`Command "${commandName}" is not registered in package.json keybindings`)
      }
    })
  })
})