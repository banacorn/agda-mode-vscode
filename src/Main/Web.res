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

  // helper function for fetching release manifest without cache
  let getDownloadDescriptor = async (url, header) => {
    let fetchReleases = async () => {
      try {
        let response = await Fetch.fetch(
          url,
          {
            method: #GET,
            mode: #cors,
            redirect: #follow,
            credentials: #omit, // important: no cookies/credentials
            headers: Fetch.Headers.make(Fetch.Headers.Init.object(header)), // optional but recommended
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

    switch await fetchReleases() {
    | Error(error) => Error(error)
    | Ok(releases) =>
      // we want only the "dev" release
      let pinnedRelease =
        releases->Array.find(release =>
          release.tag_name == "dev" || release.name == "Development Release (dev)"
        )

      switch pinnedRelease {
      | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      | Some(pinnedRelease) =>
        // the WASM asset is named "als-dev-wasm" at the moment
        let result = pinnedRelease.assets->Array.find(asset => asset.name == "als-dev-wasm")
        switch result {
        | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        | Some(asset) =>
          Ok({
            Connection__Download__GitHub.DownloadDescriptor.release: pinnedRelease,
            asset,
            saveAsFileName: "dev-als",
          })
        }
      }
    }
  }

  let getDownloadDescriptorOfDevALS = (_globalStorageUri, _platform) =>
    getDownloadDescriptor(
      "https://api.github.com/repos/banacorn/agda-language-server/releases",
      {"Accept": "application/vnd.github+json"},
    )
  let getDownloadDescriptorOfDevWASMALS = _globalStorageUri =>
    getDownloadDescriptor(
      "https://api.github.com/repos/banacorn/agda-language-server/releases",
      {"Accept": "application/vnd.github+json"},
    )
  let getDownloadDescriptorOfLatestALS = async (_, _, _) => Error(
    Connection__Download.Error.CannotFindCompatibleALSRelease,
  )

  let download = (_memento, _globalStorageUri) =>
    Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

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
