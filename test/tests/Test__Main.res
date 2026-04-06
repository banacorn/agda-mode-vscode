open Mocha

let wait = ms => Promise.make((resolve, _) => Js.Global.setTimeout(resolve, ms)->ignore)

describe("Main", () => {
  describe("activateWithoutContext — in-flight cleanup wiring", () => {
    Async.it(
      "should clean up in-flight download files on startup",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "wiring-inflight-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let inFlightFile = NodeJs.Path.join([tempDir, "in-flight.download"])
        let inFlightZipFile = NodeJs.Path.join([tempDir, "in-flight.download.zip"])
        NodeJs.Fs.writeFileSync(inFlightFile, NodeJs.Buffer.fromString("downloading..."))
        NodeJs.Fs.writeFileSync(inFlightZipFile, NodeJs.Buffer.fromString("downloading..."))

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let memento = Memento.make(None)
        let disposables = []
        let extensionUri = VSCode.Uri.file(NodeJs.Path.join([NodeJs.Os.tmpdir(), "ext"]))
        let _channels = Main.activateWithoutContext(
          Desktop.make(),
          disposables,
          extensionUri,
          globalStorageUri,
          memento,
        )

        // cleanupInFlightFiles is fire-and-forget — wait for async FS ops to complete
        await wait(200)

        let inFlightGone = !NodeJs.Fs.existsSync(inFlightFile)
        let inFlightZipGone = !NodeJs.Fs.existsSync(inFlightZipFile)

        let _ = await FS.deleteRecursive(globalStorageUri)

        Assert.deepStrictEqual(inFlightGone, true)
        Assert.deepStrictEqual(inFlightZipGone, true)
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
