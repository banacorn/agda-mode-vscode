// Web-specific entry point that creates Platform.makeWeb() dependencies
// This file will be used for the web bundle

// Web implementation - provides mock/limited functionality for browser environment
module Web: Platform.PlatformOps = {
  let determinePlatform = () => {
    Promise.resolve(Ok(Connection__Download__Platform.Web))
  }

  let findCommand = (_command, ~timeout as _timeout=1000) =>
    Promise.resolve(Error(Connection__Command.Error.NotFound))

  let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

  let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
    Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

  let getFetchSpec = async (_memento, _globalStorageUri, platform) => {
    let fetchReleases = async () => {
      try {
        let response = await Fetch.fetch(
          "https://api.github.com/repos/agda/agda-language-server/releases",
          {
            method: #GET,
            mode: #cors,
            redirect: #follow,
            credentials: #omit, // important: no cookies/credentials
            headers: Fetch.Headers.make(
              Fetch.Headers.Init.object({"Accept": "application/vnd.github+json"}),
            ), // optional but recommended
          },
        )
        if Fetch.Response.ok(response) {
          let json = await Fetch.Response.json(response)
          switch json->Connection__Download__GitHub.Release.decodeReleases {
          | Ok(releases) => Ok(releases)
          | Error(decodeError) =>
            Error(Connection__Download.Error.CannotFetchALSReleases(decodeError))
          }
        } else {
          let serverError = Obj.magic({"message": "GitHub API request failed"})
          Error(
            Connection__Download.Error.CannotFetchALSReleases(
              Connection__Download__GitHub.Error.CannotGetReleases(
                Connection__Download__Util.Error.ServerResponseError(serverError),
              ),
            ),
          )
        }
      } catch {
      | _ =>
        let networkError = Obj.magic({"message": "Network error"})
        Error(
          Connection__Download.Error.CannotFetchALSReleases(
            Connection__Download__GitHub.Error.CannotGetReleases(
              Connection__Download__Util.Error.ServerResponseError(networkError),
            ),
          ),
        )
      }
    }

    let chooseAssetByPlatform = (release: Connection__Download__GitHub.Release.t, platform): array<
      Connection__Download__GitHub.Asset.t,
    > => {
      let assetName = Connection__Download__Platform.toAssetName(platform)
      release.assets->Array.filter(asset => asset.name->String.endsWith(assetName ++ ".zip"))
    }

    switch await fetchReleases() {
    | Error(error) => Error(error)
    | Ok(releases) =>
      // Filter for releases after 2024-12-18 like the desktop version
      let laterReleases =
        releases->Array.filter(release =>
          Date.fromString(release.published_at) >= Date.fromString("2024-12-18")
        )

      // Use the pinned release like desktop version
      let pinnedRelease = laterReleases->Array.find(release => release.name == "v0.2.7.0.1.5")

      switch pinnedRelease {
      | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      | Some(pinnedRelease) =>
        let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
          asset.name
          ->String.replaceRegExp(%re("/als-Agda-/"), "")
          ->String.replaceRegExp(%re("/-.*/"), "")

        let assets = chooseAssetByPlatform(pinnedRelease, platform)
        let result =
          assets
          ->Array.toSorted((a, b) => Util.Version.compare(getAgdaVersion(b), getAgdaVersion(a)))
          ->Array.map(asset => {
            Connection__Download__GitHub.FetchSpec.release: pinnedRelease,
            asset,
            saveAsFileName: "latest-als",
          })
          ->Array.get(0)

        switch result {
        | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        | Some(fetchSpec) => Ok(fetchSpec)
        }
      }
    }
  }

  let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.No)

  let openFolder = _uri => Promise.resolve()
}

// Create platform dependencies for web environment
let make = (): Platform.t => module(Web)

// this function is the entry point for the web extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
