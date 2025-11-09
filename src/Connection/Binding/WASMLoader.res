// WASM Loader bindings for the qbane.als-wasm-loader extension

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

// Extract ZIP to memfs using createDirectory and createFile API
// Note: Can't use extension's memfsUnzip due to JSZip bug (file._data.uncompressedSize is undefined)
let extractZipToMemfs = %raw(`
  async function(memfs, zipData, createDirectory, createFile) {
    const JSZip = require("jszip");
    const zip = await JSZip.loadAsync(zipData);

    for (const [relativePath, file] of Object.entries(zip.files)) {
      if (file.dir) {
        try {
          createDirectory(memfs, relativePath.slice(0, -1));
        } catch (e) {
          // Directory might already exist
        }
      } else {
        const content = await file.async('uint8array');
        createFile(memfs, relativePath, {
          size: BigInt(content.length),
          reader: () => Promise.resolve(content)
        });
      }
    }
  }
`)

// Download Agda standard library for web/WASM
let downloadStdlib = async () => {
  try {
    let response = await Fetch.fetch(
      "https://raw.githubusercontent.com/banacorn/agda-library-proxy/master/agda-stdlib-2.3.zip",
      {
        method: #GET,
        headers: Fetch.Headers.make(Fetch.Headers.Init.object({"User-Agent": "agda-mode-vscode"})),
      },
    )
    Fetch.Response.ok(response) ? Ok(await bytes(response)) : Error()
  } catch {
  | Exn.Error(_) => Error()
  }
}

let make = async (extension, raw: Uint8Array.t) => {
  let exports = VSCode.Extension.exports(extension)
  let agdaLanguageServerFactory: agdaLanguageServerFactory = exports["AgdaLanguageServerFactory"]
  let wasmAPILoader: wasmAPILoader = exports["WasmAPILoader"]
  let createUriConverters = exports["createUriConverters"]

  let wasm = wasmAPILoader->load
  let mod = await WebAssembly.compile(raw)
  let factory = createFactory(agdaLanguageServerFactory, wasm, mod)
  let memfsAgdaDataDir = await wasm->createMemoryFileSystem

  // Populate memfsAgdaDataDir with standard library
  switch await downloadStdlib() {
  | Ok(zipData) =>
      try {
        await extractZipToMemfs(memfsAgdaDataDir, zipData, createDirectory, createFile)
      } catch {
      | Exn.Error(_) => ()
      }
  | Error() => ()
  }

  // Setup libraries file in memfsHome pointing to stdlib
  let presetupCallback = async ({memfsTempDir: _, memfsHome}) => {
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
    } catch {
    | Exn.Error(_) => ()
    }
  }

  {factory, wasm, memfsAgdaDataDir, createUriConverters, presetupCallback}
}
