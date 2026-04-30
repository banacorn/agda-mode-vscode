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
})
