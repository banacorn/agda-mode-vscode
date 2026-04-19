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

    it("should fall back to older release when newest lacks a compatible asset", () => {
      let makeAsset = (tag, name): Connection__Download__GitHub.Asset.t => {
        url: "https://github.com/agda/agda-language-server/releases/download/" ++ tag ++ "/" ++ name,
        id: 1,
        node_id: "",
        name,
        label: None,
        content_type: "application/zip",
        state: "uploaded",
        size: 1000000,
        created_at: "2025-01-01T00:00:00Z",
        updated_at: "2025-01-01T00:00:00Z",
        browser_download_url: "https://github.com/agda/agda-language-server/releases/download/" ++ tag ++ "/" ++ name,
      }

      let makeRelease = (tag, published_at, assets): Connection__Download__GitHub.Release.t => {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 1,
        node_id: "",
        tag_name: tag,
        target_commitish: "main",
        name: tag,
        draft: false,
        prerelease: false,
        created_at: published_at,
        published_at,
        assets,
        tarball_url: "",
        zipball_url: "",
        body: None,
      }

      // v7 is newer but has no windows asset; v6 has a windows asset
      let v7Release = makeRelease("v7", "2025-06-01T00:00:00Z", [
        makeAsset("v7", "als-v7-Agda-2.9.0-ubuntu.zip"),
      ])
      let v6Release = makeRelease("v6", "2025-01-01T00:00:00Z", [
        makeAsset("v6", "als-v6-Agda-2.8.0-windows.zip"),
      ])

      let releases = [v7Release, v6Release]

      let result = Connection__LatestALS.toDownloadOrder(releases, Connection__Download__Platform.Windows)

      switch result {
      | Error(_) => Assert.fail("expected Ok but got Error")
      | Ok(Connection__Download.Source.FromGitHub(_, descriptor)) =>
        Assert.deepStrictEqual(descriptor.release.name, "v6")
        Assert.deepStrictEqual(descriptor.asset.name, "als-v6-Agda-2.8.0-windows.zip")
      | Ok(_) => Assert.fail("expected FromGitHub source")
      }
    })

    it("should skip prerelease/draft releases and pick stable v6", () => {
      let makeAsset = (tag, name): Connection__Download__GitHub.Asset.t => {
        url: "https://github.com/agda/agda-language-server/releases/download/" ++ tag ++ "/" ++ name,
        id: 1,
        node_id: "",
        name,
        label: None,
        content_type: "application/zip",
        state: "uploaded",
        size: 1000000,
        created_at: "2025-01-01T00:00:00Z",
        updated_at: "2025-01-01T00:00:00Z",
        browser_download_url: "https://github.com/agda/agda-language-server/releases/download/" ++ tag ++ "/" ++ name,
      }

      // v8-draft is a draft with a matching windows asset — should be skipped
      let v8DraftRelease: Connection__Download__GitHub.Release.t = {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 3,
        node_id: "",
        tag_name: "v8-draft",
        target_commitish: "main",
        name: "v8-draft",
        draft: true,
        prerelease: false,
        created_at: "2025-09-01T00:00:00Z",
        published_at: "2025-09-01T00:00:00Z",
        assets: [makeAsset("v8-draft", "als-v8-draft-Agda-3.0.0-windows.zip")],
        tarball_url: "",
        zipball_url: "",
        body: None,
      }
      // v7-rc1 is a prerelease with a matching windows asset — should be skipped
      let v7PreRelease: Connection__Download__GitHub.Release.t = {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 2,
        node_id: "",
        tag_name: "v7-rc1",
        target_commitish: "main",
        name: "v7-rc1",
        draft: false,
        prerelease: true,
        created_at: "2025-06-01T00:00:00Z",
        published_at: "2025-06-01T00:00:00Z",
        assets: [makeAsset("v7-rc1", "als-v7-rc1-Agda-2.9.0-windows.zip")],
        tarball_url: "",
        zipball_url: "",
        body: None,
      }
      // v6 is stable with a matching windows asset — should be selected
      let v6Release: Connection__Download__GitHub.Release.t = {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 1,
        node_id: "",
        tag_name: "v6",
        target_commitish: "main",
        name: "v6",
        draft: false,
        prerelease: false,
        created_at: "2025-01-01T00:00:00Z",
        published_at: "2025-01-01T00:00:00Z",
        assets: [makeAsset("v6", "als-v6-Agda-2.8.0-windows.zip")],
        tarball_url: "",
        zipball_url: "",
        body: None,
      }

      let releases = [v8DraftRelease, v7PreRelease, v6Release]

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
