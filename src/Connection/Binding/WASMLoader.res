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

// Download standard library for web/WASM
let downloadStdlib = async () => {
  // URL to agda-stdlib 2.3 from the proxy repo
  let stdlibUrl = "https://raw.githubusercontent.com/banacorn/agda-library-proxy/master/agda-stdlib-2.3.zip"

  Util.log("[WASMLoader] Starting stdlib download from", stdlibUrl)

  try {
    // Fetch the stdlib archive
    let response = await Fetch.fetch(
      stdlibUrl,
      {
        method: #GET,
        headers: Fetch.Headers.make(Fetch.Headers.Init.object({"User-Agent": "agda-mode-vscode"})),
      },
    )

    if !Fetch.Response.ok(response) {
      let errorMsg = "Failed to fetch stdlib: HTTP " ++ string_of_int(Fetch.Response.status(response))
      Util.log("[WASMLoader] Download failed", errorMsg)
      Error(errorMsg)
    } else {
      Util.log("[WASMLoader] Download successful, converting to Uint8Array", ())

      // Convert response to Uint8Array
      let arrayBufferData = await arrayBuffer(response)
      let zipData = makeUint8ArrayFromArrayBuffer(arrayBufferData)

      Util.log("[WASMLoader] Stdlib data ready, size (bytes)", Core__TypedArray.length(zipData))
      Ok(zipData)
    }
  } catch {
  | Exn.Error(err) => {
      let errorMsg = "Exception while downloading stdlib: " ++ Exn.message(err)->Option.getOr("unknown error")
      Util.log("[WASMLoader] Download exception", errorMsg)
      Error(errorMsg)
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

  // TEST: Download stdlib (temporary test code)
  Util.log("[WASMLoader] Testing stdlib download...", ())
  let _ = switch await downloadStdlib() {
  | Ok(data) => Util.log("[WASMLoader] SUCCESS: Downloaded stdlib", Core__TypedArray.length(data))
  | Error(err) => Util.log("[WASMLoader] ERROR: Failed to download stdlib", err)
  }

  {factory, wasm, memfsAgdaDataDir, createUriConverters}
}
