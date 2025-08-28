open Mocha

module GitHub = Connection__Download__GitHub

describe("Connection DevALS", () => {
  This.timeout(10000)

  describe("getDownloadDescriptor integration", () => {
    Async.it(
      "should fetch dev release spec from GitHub API",
      async () => {
        let globalStorageUri = VSCode.Uri.file("/tmp/test-dev-als")

        // Try with Ubuntu platform since we know it should exist
        let platform = Connection__Download__Platform.Ubuntu
        let memento = Memento.make(None)
        let result = await Connection__DevALS.getDownloadDescriptor(
          memento,
          globalStorageUri,
          platform,
        )

        switch result {
        | Ok(downloadDescriptor) =>
          // Verify we got the dev release
          Assert.deepStrictEqual(downloadDescriptor.release.tag_name, "dev")
          // Verify the saveAsFileName is correct
          Assert.deepStrictEqual(downloadDescriptor.saveAsFileName, "dev-als")
          // Verify it's a dev asset for Ubuntu platform
          Assert.deepStrictEqual(downloadDescriptor.asset.name, "als-dev-Agda-2.8.0-ubuntu.zip")
        | Error(error) =>
          Assert.fail(
            "Expected success but got error: " ++ Connection__Download.Error.toString(error),
          )
        }
      },
    )
  })
})
