// Minimal web-compatible entry point for agda-mode VS Code extension
// Stripped down to "Hello World" level for testing web build configuration

// Simple test command registration
let registerTestCommand = (subscriptions) => {
  let disposable = VSCode.Commands.registerCommand("agda-mode.hello-web", () => {
    VSCode.Window.showInformationMessage("Hello from agda-mode web version!", [])->ignore
    Promise.resolve()
  })
  subscriptions->Array.push(disposable)->ignore
}

// Minimal activation function - Web version
let activate = context => {
  let subscriptions = VSCode.ExtensionContext.subscriptions(context)
  
  // Log successful activation
  Js.Console.log("agda-mode web version: Activating minimal version...")
  
  // Register a simple test command
  registerTestCommand(subscriptions)
  
  // Show activation message
  VSCode.Window.showInformationMessage(
    "agda-mode web version activated successfully! Try running 'agda-mode.hello-web' command.",
    []
  )->ignore
  
  Js.Console.log("agda-mode web version: Activation complete!")
  
  // Return unit (no channels needed for minimal version)
  ()
}

let deactivate = () => {
  Js.Console.log("agda-mode web version: Deactivating...")
}