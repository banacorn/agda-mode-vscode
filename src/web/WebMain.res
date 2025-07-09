// Web-compatible entry point for agda-mode VS Code extension
// Registers all agda-mode commands as stubs for web compatibility

let registerAllCommands = (subscriptions) => {
  // Register test command
  let testDisposable = VSCode.Commands.registerCommand("agda-mode.hello-web", () => {
    VSCode.Window.showInformationMessage("Hello from agda-mode web version!", [])->ignore
    Promise.resolve()
  })
  subscriptions->Array.push(testDisposable)->ignore

  // Register all agda-mode commands as stubs
  Command.names->Array.forEach(((_, commandName)) => {
    let disposable = VSCode.Commands.registerCommand("agda-mode." ++ commandName, () => {
      VSCode.Window.showInformationMessage(
        `agda-mode web: Command '${commandName}' is not yet implemented in the web version. This is a placeholder.`,
        []
      )->ignore
      Promise.resolve()
    })
    subscriptions->Array.push(disposable)->ignore
  })
}

// Web version activation function
let activate = context => {
  let subscriptions = VSCode.ExtensionContext.subscriptions(context)
  
  // Log successful activation
  Js.Console.log("agda-mode web version: Activating with all command stubs...")
  
  // Register all commands
  registerAllCommands(subscriptions)
  
  // Show activation message
  VSCode.Window.showInformationMessage(
    `agda-mode web version activated successfully! ${Array.length(Command.names)->Int.toString} commands registered as stubs. Try 'agda-mode.hello-web' for testing.`,
    []
  )->ignore
  
  Js.Console.log("agda-mode web version: Activation complete!")
  
  // Return unit
  ()
}

let deactivate = () => {
  Js.Console.log("agda-mode web version: Deactivating...")
}