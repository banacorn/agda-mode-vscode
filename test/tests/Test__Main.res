open Mocha

describe("Main", () => {
  describe("activateWithoutContext — in-flight cleanup wiring", () => {
    it(
      "should call startupCleanup with globalStorageUri",
      () => {
        let calledWith = ref(None)
        let mockCleanup = uri => {
          calledWith := Some(VSCode.Uri.toString(uri))
          Promise.resolve()
        }

        let globalStorageUri = VSCode.Uri.file(NodeJs.Path.join([NodeJs.Os.tmpdir(), "wiring-inflight-test"]))
        let memento = Memento.make(None)
        let disposables = []
        let extensionUri = VSCode.Uri.file(NodeJs.Path.join([NodeJs.Os.tmpdir(), "ext"]))
        let _ = try {
          Main.activateWithoutContext(
            Desktop.make(), disposables, extensionUri, globalStorageUri, memento,
            ~startupCleanup=mockCleanup,
          )->ignore
        } catch {
        | Js.Exn.Error(obj) if Js.Exn.message(obj)->Option.getOr("")->String.includes("already exists") => ()
        | exn => raise(exn)
        }

        Assert.deepStrictEqual(calledWith.contents, Some(VSCode.Uri.toString(globalStorageUri)))
      },
    )
  })

  describe("normalizeGlobalStorageUri", () => {
    it(
      "should preserve desktop VS Code globalStorageUri under Code/User/globalStorage",
      () => {
        let uri = VSCode.Uri.file(
          "/Users/banacorn/Library/Application Support/Code/User/globalStorage/banacorn.agda-mode",
        )

        Assert.deepStrictEqual(
          Main.normalizeGlobalStorageUri(uri)->VSCode.Uri.toString,
          uri->VSCode.Uri.toString,
        )
      },
    )

    it(
      "should correct malformed root User/globalStorage path to Users/globalStorage",
      () => {
        let malformed = VSCode.Uri.file("/User/globalStorage/banacorn.agda-mode")
        let corrected = VSCode.Uri.file("/Users/globalStorage/banacorn.agda-mode")

        Assert.deepStrictEqual(
          Main.normalizeGlobalStorageUri(malformed)->VSCode.Uri.toString,
          corrected->VSCode.Uri.toString,
        )
      },
    )

    it(
      "should not rewrite User/globalStorage when it appears in the middle of a valid desktop path",
      () => {
        let uri = VSCode.Uri.file(
          "/Users/banacorn/Library/Application Support/Code/User/globalStorage/banacorn.agda-mode/hardcoded-als",
        )

        Assert.deepStrictEqual(
          Main.normalizeGlobalStorageUri(uri)->VSCode.Uri.toString,
          uri->VSCode.Uri.toString,
        )
      },
    )
  })
})
