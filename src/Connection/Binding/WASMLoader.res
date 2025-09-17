// WASM Loader bindings for the qbane.als-wasm-loader extension

// Core WASM types
type wasmAPILoader
type wasmAPI
type memoryFileSystem
type wasmModule
type agdaLanguageServerFactory
type agdaLanguageServer

// URI converters type (opaque)
type uriConverters

// Setup result type
type t = {
  factory: agdaLanguageServerFactory,
  wasm: wasmAPI,
  memfsAgdaDataDir: memoryFileSystem,
  createUriConverters: unit => uriConverters,
}

// Server options (based on README pattern)
type serverOptions = {
  // Configuration options for the server
}

// External bindings for WASM loader operations
@send external load: wasmAPILoader => wasmAPI = "load"
@send
external createMemoryFileSystem: wasmAPI => promise<memoryFileSystem> = "createMemoryFileSystem"

// Binding for prepareMemfsFromAgdaDataZip function
external prepareMemfsFromAgdaDataZip: (
  Uint8Array.t,
  memoryFileSystem,
) => promise<memoryFileSystem> = "prepareMemfsFromAgdaDataZip"

// Factory creation helper
let createFactory = %raw(`function(constructor, wasm, mod) { return new constructor(wasm, mod); }`)

// Additional bindings for language server functionality
@send
external createServer: (
  agdaLanguageServerFactory,
  memoryFileSystem,
  serverOptions,
) => agdaLanguageServer = "createServer"

// Agda data directory in memory filesystem for Agda-2.7.0.1
let prepareAgdaDataDir = async (extension, memfs: memoryFileSystem, dataPath: string) => {
  let agdaDataZipPath = VSCode.Uri.file(dataPath)

  switch await FS.readFile(agdaDataZipPath) {
  | Error(_) => Error("Failed to read agda-data.zip from " ++ dataPath)
  | Ok(zipData) =>
    try {
      // Get the function from extension exports
      let exports = VSCode.Extension.exports(extension)
      let _ = await exports["prepareMemfsFromAgdaDataZip"](zipData, memfs)
      Ok()
    } catch {
    | Exn.Error(_) => Error("Failed to prepare memory filesystem from agda-data.zip")
    }
  }
}

let make = async (extension, raw: Uint8Array.t) => {
  // Get the exports from the extension
  let exports = VSCode.Extension.exports(extension)

  // Access exports directly
  let agdaLanguageServerFactory: agdaLanguageServerFactory = exports["AgdaLanguageServerFactory"]
  let wasmAPILoader: wasmAPILoader = exports["WasmAPILoader"]
  let createUriConverters = exports["createUriConverters"]

  // Load WASM API and compile module
  let wasm = wasmAPILoader->load
  let mod = await WebAssembly.compile(raw)

  // Create language server factory
  let factory = createFactory(agdaLanguageServerFactory, wasm, mod)
  let memfsAgdaDataDir = await wasm->createMemoryFileSystem

  {factory, wasm, memfsAgdaDataDir, createUriConverters}
}
