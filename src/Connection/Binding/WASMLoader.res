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

// Binding for arrayBuffer method
@send external arrayBuffer: Fetch.Response.t => promise<ArrayBuffer.t> = "arrayBuffer"

// Binding for creating Uint8Array from ArrayBuffer
@new external makeUint8ArrayFromArrayBuffer: ArrayBuffer.t => Uint8Array.t = "Uint8Array"

// Factory creation helper
let createFactory = %raw(`function(constructor, wasm, mod) { return new constructor(wasm, mod); }`)

// Additional bindings for language server functionality
@send
external createServer: (
  agdaLanguageServerFactory,
  memoryFileSystem,
  serverOptions,
) => agdaLanguageServer = "createServer"

// Agda data directory in memory filesystem for Agda-2.7.0 (provisional - will be removed)
let prepareAgdaDataDir = async (extension, memfs: memoryFileSystem) => {
  // Hardcoded GitHub release URL - all download logic contained here for easy removal
  let agdaDataUrl = "https://github.com/andy0130tw/vscode-als-wasm-loader/releases/download/v0.2.1/agda-data.zip"

  try {
    // Fetch the zip from GitHub
    let response = await Fetch.fetch(
      agdaDataUrl,
      {
        method: #GET,
        headers: Fetch.Headers.make(Fetch.Headers.Init.object({"User-Agent": "agda-mode-vscode"})),
      },
    )
    if !Fetch.Response.ok(response) {
      Error(
        "Failed to fetch agda-data.zip from GitHub: HTTP " ++
        string_of_int(Fetch.Response.status(response)),
      )
    } else {
      // Convert response to Uint8Array
      let arrayBufferData = await arrayBuffer(response)
      let zipData = makeUint8ArrayFromArrayBuffer(arrayBufferData)

      // Get the function from extension exports and populate memory filesystem
      let exports = VSCode.Extension.exports(extension)
      let _ = await exports["prepareMemfsFromAgdaDataZip"](zipData, memfs)

      // Return a dictionary of "env", set Agda_datadir to /opt/agda where the data is mounted in WASM
      Ok(Dict.fromArray([("Agda_datadir", "/opt/agda")]))
    }
  } catch {
  | Exn.Error(_) => Error("Failed to download or prepare agda-data.zip")
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
