open Mocha

module GitHub = Connection__Download__GitHub

describe("Connection DevALS", () => {
  This.timeout(10000)

  describe("GitHub API integration", () => {
    Async.it(
      "should fetch dev release from GitHub API",
      async () => {
        let globalStorageUri = VSCode.Uri.file("/tmp/test-dev-als")

        let releaseResult = await Connection__DevALS.getALSReleaseManifestWithoutCache(
          globalStorageUri,
        )

        switch releaseResult {
        | Error(e) =>
          Assert.fail("Failed to fetch releases: " ++ Connection__Download.Error.toString(e))
        | Ok(releases) =>
          // Should find dev release
          let devRelease = releases->Array.find(release => release.tag_name == "dev")
          switch devRelease {
          | None => Assert.fail("Dev release not found")
          | Some(release) => Assert.deepStrictEqual(release.tag_name, "dev")
          }
        }
      },
    )
  })

  describe("getDownloadDescriptor integration", () => {
    Async.it(
      "should fetch dev release spec from GitHub API",
      async () => {
        let globalStorageUri = VSCode.Uri.file("/tmp/test-dev-als")

        // Try with Ubuntu platform since we know it should exist
        let platform = Connection__Download__Platform.Ubuntu
        let result = await Connection__DevALS.getDownloadDescriptor(globalStorageUri, platform)

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
