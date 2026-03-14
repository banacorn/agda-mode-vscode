// WASM Loader bindings for the qbane.als-wasm-loader extension

open VSCode

// Core WASM types
type wasmAPILoader
type wasmAPI
type memoryFileSystem
type agdaLanguageServerFactory
type uriConverters

// Type for presetupCallback
type presetupCallbackArgs = {
  memfsTempDir: memoryFileSystem,
  memfsHome: memoryFileSystem,
}
type presetupCallback = presetupCallbackArgs => promise<unit>

// Setup result type
type t = {
  factory: agdaLanguageServerFactory,
  wasm: wasmAPI,
  memfsAgdaDataDir: memoryFileSystem,
  createUriConverters: unit => uriConverters,
  presetupCallback: presetupCallback,
}

// External bindings
@send external load: wasmAPILoader => wasmAPI = "load"
@send external createMemoryFileSystem: wasmAPI => promise<memoryFileSystem> = "createMemoryFileSystem"
@send external bytes: Fetch.Response.t => promise<Uint8Array.t> = "bytes"
@send external createDirectory: (memoryFileSystem, string) => unit = "createDirectory"

let createFile: (memoryFileSystem, string, 'a) => unit = %raw(`
  function(memfs, path, options) {
    memfs.createFile(path, options);
  }
`)

let createFactory = %raw(`function(constructor, wasm, mod) { return new constructor(wasm, mod); }`)

// Download Agda standard library for web/WASM
let downloadStdlib = async () => {
  try {
    let response = await Fetch.fetch(
      "https://raw.githubusercontent.com/banacorn/agda-library-proxy/master/agda-stdlib-2.3.zip",
      // Cannot send additional headers because GitHub responds 403 to the CORS preflight request
      {
        // method: #GET,
        // headers: Fetch.Headers.make(Fetch.Headers.Init.object({"User-Agent": "agda-mode-vscode"})),
      },
    )
    if Fetch.Response.ok(response) {
      Ok(await bytes(response))
    } else {
      Exn.raiseError("invalid status code " ++ response->Fetch.Response.status->Belt.Int.toString)
    }
  } catch {
  | Exn.Error(err) => Error(("Error fetching archive.", err))
  }
}

let make = async (extension, raw: Uint8Array.t) => {
  let exports = VSCode.Extension.exports(extension)
  let agdaLanguageServerFactory: agdaLanguageServerFactory = exports["AgdaLanguageServerFactory"]
  let wasmAPILoader: wasmAPILoader = exports["WasmAPILoader"]
  let createUriConverters = exports["createUriConverters"]
  let memfsUnzip: (memoryFileSystem, Uint8Array.t) => promise<memoryFileSystem> = exports["memfsUnzip"]

  let wasm = wasmAPILoader->load
  let mod = await WebAssembly.compile(raw)
  let factory = createFactory(agdaLanguageServerFactory, wasm, mod)
  let memfsAgdaDataDir = await wasm->createMemoryFileSystem

  // Populate memfsAgdaDataDir with standard library using extension's memfsUnzip
  let stdlibSetupResult = switch await downloadStdlib() {
  | Error(err) => Error(err)
  | Ok(zipData) =>
      try {
        let _ = await memfsUnzip(memfsAgdaDataDir, zipData)
        Ok()
      } catch {
      | Exn.Error(err) => Error(("Error unzipping downloaded archive.", err))
      }
  }

  // Setup libraries file in memfsHome pointing to stdlib
  let presetupCallback = async ({memfsTempDir: _, memfsHome}) => {
    let memfsSetupResult = switch stdlibSetupResult {
    | Error(err) => Error(err)
    | Ok() =>
      try {
        createDirectory(memfsHome, ".config")
        createDirectory(memfsHome, ".config/agda")
        let _ = %raw(`
          function(memfsHome, createFile) {
            const content = new TextEncoder().encode('/opt/agda/agda-stdlib-2.3/standard-library.agda-lib\n');
            createFile(memfsHome, '.config/agda/libraries', {
              size: BigInt(content.length),
              reader: () => Promise.resolve(content)
            });
          }
        `)(memfsHome, createFile)
        Ok()
      } catch {
      | Exn.Error(err) => Error(("Error extract unzipped content to memfs.", err))
      }
    }

    switch memfsSetupResult {
    | Ok() => ()
    | Error((desc, exn)) => {
      let exnMsg = exn->Exn.message->Option.getOr("N/A")
      let _ = Window.showWarningMessage(
        "Failed to download Agda standard library. " ++ desc ++ " Will continue without stdlib: " ++ exnMsg,
        [])
      ()
    }
    }
  }

  {factory, wasm, memfsAgdaDataDir, createUriConverters, presetupCallback}
}
