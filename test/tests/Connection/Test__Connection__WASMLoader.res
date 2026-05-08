open Mocha

let stdlibLibFile = "/opt/agda/agda-stdlib-2.3/standard-library.agda-lib\n"

let emptyManagedLibraries: WASMLoader.installedLibrariesDesc = {
  base: "$ALSWASM_GLOBAL_STORE_DIR",
  prefix: "vscode-userdata://global-storage/qbane.als-wasm-loader",
  paths: [],
}

let standardLibraryManagedLibraries: WASMLoader.installedLibrariesDesc = {
  base: "$ALSWASM_GLOBAL_STORE_DIR",
  prefix: "vscode-userdata://global-storage/qbane.als-wasm-loader",
  paths: ["standard-library-v2.3/standard-library.agda-lib"],
}

let validWasmModuleBytes: Uint8Array.t = %raw(`new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0])`)
let fakeZipBytes: Uint8Array.t = %raw(`new Uint8Array([1, 2, 3])`)

let makeMemoryFileSystem: unit => WASMLoader.memoryFileSystem = %raw(`
  () => ({
    directories: [],
    files: {},
    createDirectory(path) {
      this.directories.push(path);
    },
    createFile(path, content) {
      this.files[path] = new TextDecoder().decode(content);
    },
  })
`)

let readMemoryFile: (WASMLoader.memoryFileSystem, string) => option<string> = %raw(`
  (memfs, path) => Object.prototype.hasOwnProperty.call(memfs.files, path)
    ? memfs.files[path]
    : undefined
`)

let makeWasmAPI = (): WASMLoader.wasmAPI => %raw(`
  {
    createMemoryFileSystem() {
      return Promise.resolve(makeMemoryFileSystem());
    },
  }
`)

let makeLoaderExtension = (desc, memfsUnzip, wasm): VSCode.Extension.t<'a> => %raw(`
  (desc, memfsUnzip, wasm) => ({
    exports: {
      AgdaLanguageServerFactory: class {
        constructor(wasm, mod) {
          this.wasm = wasm;
          this.mod = mod;
        }
      },
      WasmAPILoader: {
        load() {
          return wasm;
        },
      },
      createUriConverters() {
        return {};
      },
      memfsUnzip,
      listInstalledLibraries() {
        return Promise.resolve(desc);
      },
    },
  })
`)(desc, memfsUnzip, wasm)

describe("Connection__Binding__WASMLoader", () => {
  describe("resolveLibrarySetup", () => {
    Async.it("should use stdlib fallback when the loader has no library API", async () => {
      Assert.deepStrictEqual(await WASMLoader.resolveLibrarySetup(None), WASMLoader.StdlibFallback)
    })

    Async.it("should use stdlib fallback when the loader reports no installed libraries", async () => {
      Assert.deepStrictEqual(
        await WASMLoader.resolveLibrarySetup(Some(() => Promise.resolve(emptyManagedLibraries))),
        WASMLoader.StdlibFallback,
      )
    })

    Async.it("should use loader-managed libraries when the loader reports installed libraries", async () => {
      Assert.deepStrictEqual(
        await WASMLoader.resolveLibrarySetup(Some(() =>
          Promise.resolve(standardLibraryManagedLibraries)
        )),
        WASMLoader.Managed(standardLibraryManagedLibraries),
      )
    })
  })

  describe("make", () => {
    beforeEach(() => WASMLoader.clearMemfsAgdaDataDirCacheForTests())

    Async.it("should provision fallback stdlib when loader-managed libraries are empty", async () => {
      let unzipCount = ref(0)
      let memfsUnzip = (memfs, _data) => {
        unzipCount.contents = unzipCount.contents + 1
        Promise.resolve(memfs)
      }
      let downloadCount = ref(0)
      let downloadStdlib = () => {
        downloadCount.contents = downloadCount.contents + 1
        Promise.resolve(Ok(fakeZipBytes))
      }

      let wasmSetup = await WASMLoader.makeWithStdlibDownloader(
        makeLoaderExtension(emptyManagedLibraries, memfsUnzip, makeWasmAPI()),
        validWasmModuleBytes,
        downloadStdlib,
      )

      let memfsHome = makeMemoryFileSystem()
      await wasmSetup.presetupCallback({
        memfsTempDir: makeMemoryFileSystem(),
        memfsHome,
      })

      Assert.deepStrictEqual(downloadCount.contents, 1)
      Assert.deepStrictEqual(unzipCount.contents, 1)
      Assert.deepStrictEqual(readMemoryFile(memfsHome, ".config/agda/libraries"), Some(stdlibLibFile))
      Assert.deepStrictEqual(readMemoryFile(memfsHome, ".config/agda/defaults"), Some("standard-library"))
    })

    Async.it("should use loader-managed library paths when libraries are installed", async () => {
      let unzipCount = ref(0)
      let memfsUnzip = (memfs, _data) => {
        unzipCount.contents = unzipCount.contents + 1
        Promise.resolve(memfs)
      }
      let downloadCount = ref(0)
      let downloadStdlib = () => {
        downloadCount.contents = downloadCount.contents + 1
        Promise.resolve(Ok(fakeZipBytes))
      }

      let wasmSetup = await WASMLoader.makeWithStdlibDownloader(
        makeLoaderExtension(standardLibraryManagedLibraries, memfsUnzip, makeWasmAPI()),
        validWasmModuleBytes,
        downloadStdlib,
      )

      let memfsHome = makeMemoryFileSystem()
      await wasmSetup.presetupCallback({
        memfsTempDir: makeMemoryFileSystem(),
        memfsHome,
      })

      Assert.deepStrictEqual(downloadCount.contents, 0)
      Assert.deepStrictEqual(unzipCount.contents, 0)
      Assert.deepStrictEqual(
        readMemoryFile(memfsHome, ".config/agda/libraries"),
        Some("$ALSWASM_GLOBAL_STORE_DIR/standard-library-v2.3/standard-library.agda-lib\n"),
      )
      Assert.deepStrictEqual(readMemoryFile(memfsHome, ".config/agda/defaults"), None)
    })
  })
})
