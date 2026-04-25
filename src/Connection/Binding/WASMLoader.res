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

type installedLibrariesDesc = {
  base: string,
  prefix: string,
  paths: array<string>,
}

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

let isDesktop = %raw("Vscode.env.uiKind === Vscode.UIKind.Desktop")
let memfsAgdaDataDirCache = ref(None)

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
      isDesktop ? {
        method: #GET,
        headers: Fetch.Headers.make(Fetch.Headers.Init.object({"User-Agent": "agda-mode-vscode"})),
      } :
      // Cannot send additional headers because GitHub responds 403 to the CORS preflight request
      {},
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

let libsDescToLines = (desc: installedLibrariesDesc) =>
  desc.paths->Array.map(path => desc.base ++ "/" ++ path ++ "\n")->Array.join("")

let make = async (extension, raw: Uint8Array.t) => {
  let exports = VSCode.Extension.exports(extension)
  let agdaLanguageServerFactory: agdaLanguageServerFactory = exports["AgdaLanguageServerFactory"]
  let wasmAPILoader: wasmAPILoader = exports["WasmAPILoader"]
  let createUriConverters = exports["createUriConverters"]
  let memfsUnzip: (memoryFileSystem, Uint8Array.t) => promise<memoryFileSystem> = exports["memfsUnzip"]
  let listInstalledLibraries: option<unit => promise<installedLibrariesDesc>> = exports["listInstalledLibraries"]

  let wasm = wasmAPILoader->load
  let mod = await WebAssembly.compile(raw)
  let factory = createFactory(agdaLanguageServerFactory, wasm, mod)

  let (memfsAgdaDataDir, stdlibSetupResult) = {
    switch memfsAgdaDataDirCache.contents {
    | Some(cached) => (cached, Ok())
    | None => {
      let memfsAgdaDataDir = await wasm->createMemoryFileSystem

      let stdlibSetupResult = switch listInstalledLibraries {
      // skip if having library management support
      | Some(_) => {
        memfsAgdaDataDirCache.contents = Some(memfsAgdaDataDir)
        Ok()
      }

      // Populate memfsAgdaDataDir with standard library using extension's memfsUnzip
      | None => switch await downloadStdlib() {
        | Error(err) => Error(err)
        | Ok(zipData) =>
          try {
            let _ = await memfsUnzip(memfsAgdaDataDir, zipData)
            // NOTE: Do not save cache in this path;
            // older ALS WASM Loader will stuck on setup process with unknown reason
            Ok()
          } catch {
          | Exn.Error(err) => Error(("Error unzipping downloaded archive.", err))
          }
        }
      }

      (memfsAgdaDataDir, stdlibSetupResult)
    }
    }
  }

  let libsContent: string = await listInstalledLibraries->Option.mapOr(
    // For older ALS WASM loader that does not support library management,
    // provide the only entry stdlib-2.3 from `downloadStdlib`
    Promise.resolve("/opt/agda/agda-stdlib-2.3/standard-library.agda-lib\n"),
    fn => fn()->Promise.thenResolve(libsDescToLines))

  // Setup libraries file in memfsHome pointing to stdlib
  let presetupCallback = async ({memfsTempDir: _, memfsHome}) => {
    let doCreateFile: (string, string) => unit = %raw(`
      (memfsHome, createFile) => (path, content) =>
        createFile(memfsHome, path, new TextEncoder().encode(content))
    `)(memfsHome, createFile)

    let memfsSetupResult = switch stdlibSetupResult {
    | Error(err) => Error(err)
    | Ok() =>
      try {
        createDirectory(memfsHome, ".config")
        createDirectory(memfsHome, ".config/agda")

        doCreateFile(".config/agda/libraries", libsContent)
        switch listInstalledLibraries {
        | None => doCreateFile(".config/agda/defaults", "standard-library")
        | _ => ()
        }
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
