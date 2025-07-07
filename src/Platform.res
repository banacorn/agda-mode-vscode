// Platform detection for VS Code Web vs Desktop

// External bindings for environment detection
@val @scope("globalThis") external processVersions: option<'a> = "process.versions"

// Detect if running in VS Code Web environment
let isWeb = () => {
  // Check if we're in a web environment
  // In Node.js (desktop), process.versions exists
  // In browser (web), it doesn't
  switch processVersions {
  | Some(_) => false  // We have process.versions, so we're on desktop
  | None => true      // No process.versions, so we're in web
  }
}

// Get environment string for logging
let getEnvironment = () => {
  isWeb() ? "web" : "desktop"
}

// Check if WASM should be used
let shouldUseWasm = () => {
  // For now, WASM is only available in web environment
  // Later we can add user preference override
  isWeb()
}

// Log current environment (for debugging)
let logEnvironment = () => {
  let env = getEnvironment()
  let wasm = shouldUseWasm() ? "WASM" : "native"
  Js.Console.log(`Agda Mode running in ${env} environment, using ${wasm} ALS`)
}

// Test function to verify platform detection
let testPlatformDetection = () => {
  let webResult = isWeb()
  let envString = getEnvironment() 
  let wasmResult = shouldUseWasm()
  
  Js.Console.log("=== Platform Detection Test ===")
  Js.Console.log(`isWeb(): ${webResult ? "true" : "false"}`)
  Js.Console.log(`getEnvironment(): ${envString}`)
  Js.Console.log(`shouldUseWasm(): ${wasmResult ? "true" : "false"}`)
  Js.Console.log(`process.versions exists: ${switch processVersions { | Some(_) => "true" | None => "false" }}`)
  Js.Console.log("==============================")
}