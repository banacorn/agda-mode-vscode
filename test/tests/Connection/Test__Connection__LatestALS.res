open Mocha

describe("Connection__LatestALS", () => {
  describe("toDownloadOrder", () => {
    it("should not pin v0.2.7.0.1.5 when v6 exists", () => {
      let makeAsset = (name): Connection__Download__GitHub.Asset.t => {
        url: "https://github.com/agda/agda-language-server/releases/download/v6/" ++ name,
        id: 1,
        node_id: "",
        name,
        label: None,
        content_type: "application/zip",
        state: "uploaded",
        size: 1000000,
        created_at: "2025-01-01T00:00:00Z",
        updated_at: "2025-01-01T00:00:00Z",
        browser_download_url: "https://github.com/agda/agda-language-server/releases/download/v6/" ++ name,
      }

      let makeRelease = (name, assets): Connection__Download__GitHub.Release.t => {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 1,
        node_id: "",
        tag_name: name,
        target_commitish: "main",
        name,
        draft: false,
        prerelease: false,
        created_at: "2025-01-01T00:00:00Z",
        published_at: "2025-01-01T00:00:00Z",
        assets,
        tarball_url: "",
        zipball_url: "",
        body: None,
      }

      let pinnedRelease = makeRelease("v0.2.7.0.1.5", [
        makeAsset("als-v0.2.7.0.1.5-Agda-2.6.3-windows.zip"),
      ])
      let v6Release = makeRelease("v6", [
        makeAsset("als-v6-Agda-2.8.0-windows.zip"),
      ])

      let releases = [pinnedRelease, v6Release]

      let result = Connection__LatestALS.toDownloadOrder(releases, Connection__Download__Platform.Windows)

      switch result {
      | Error(_) => Assert.fail("expected Ok but got Error")
      | Ok(Connection__Download.Source.FromGitHub(_, descriptor)) =>
        Assert.deepStrictEqual(descriptor.release.name, "v6")
        Assert.deepStrictEqual(descriptor.asset.name, "als-v6-Agda-2.8.0-windows.zip")
      | Ok(_) => Assert.fail("expected FromGitHub source")
      }
    })
  })
})
