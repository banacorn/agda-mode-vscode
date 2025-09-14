// WASM Loader bindings for the qbane.als-wasm-loader extension

// Core WASM types
type wasmAPILoader
type wasmAPI
type memoryFileSystem 
type wasmModule
type agdaLanguageServerFactory
type agdaLanguageServer

// Setup result type
type t = {
  factory: agdaLanguageServerFactory,
  wasm: wasmAPI,
  memfsAgdaDataDir: memoryFileSystem,
}

// Server options (based on README pattern)
type serverOptions = {
  // Configuration options for the server
}

// External bindings for WASM loader operations
@send external load: wasmAPILoader => wasmAPI = "load"
@send external createMemoryFileSystem: wasmAPI => promise<memoryFileSystem> = "createMemoryFileSystem"

// Factory creation helper
let createFactory = %raw(`function(constructor, wasm, mod) { return new constructor(wasm, mod); }`)

// Additional bindings for language server functionality
@send external createServer: (agdaLanguageServerFactory, memoryFileSystem, serverOptions) => agdaLanguageServer = "createServer"

// // LSP communication methods on the server instance
// @send external sendRequest: (agdaLanguageServer, Js.Json.t) => promise<Js.Json.t> = "sendRequest"
// @send external sendNotification: (agdaLanguageServer, Js.Json.t) => promise<unit> = "sendNotification"
// @send external onRequest: (agdaLanguageServer, string, Js.Json.t => promise<result<Js.Json.t, Js.Exn.t>>) => unit = "onRequest"
// @send external onNotification: (agdaLanguageServer, string, Js.Json.t => unit) => unit = "onNotification"
// @send external onError: (agdaLanguageServer, Js.Exn.t => unit) => unit = "onError"
// @send external start: agdaLanguageServer => promise<unit> = "start"
// @send external stop: agdaLanguageServer => promise<unit> = "stop"

let make = async (extension, raw) => {
  // Get the exports from the extension
  let exports = VSCode.Extension.exports(extension)

  // Access exports directly
  let agdaLanguageServerFactory: agdaLanguageServerFactory = exports["AgdaLanguageServerFactory"]
  let wasmAPILoader: wasmAPILoader = exports["WasmAPILoader"]
  // let createUriConverters = exports["createUriConverters"]

  // Load WASM API and compile module
  let wasm = wasmAPILoader->load
  let mod = await WebAssembly.compile(raw)

  // Create language server factory
  let factory = createFactory(agdaLanguageServerFactory, wasm, mod)
  let memfsAgdaDataDir = await wasm->createMemoryFileSystem

  {factory: factory, wasm: wasm, memfsAgdaDataDir: memfsAgdaDataDir}
}
