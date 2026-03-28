open Mocha

describe("Main", () => {
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
