// Web-specific entry point that creates Platform.makeWeb() dependencies
// This file will be used for the web bundle

// Web implementation - provides mock/limited functionality for browser environment
module Web: Platform.PlatformOps = {
  let determinePlatform = () => {
    Promise.resolve(Ok(Connection__Download__Platform.Web))
  }

  let findCommand = (_command, ~timeout as _timeout=1000) =>
    Promise.resolve(Error(Connection__Command.Error.NotFound))

  let alreadyDownloaded = async (globalStorageUri, order) => {
    switch order {
    | Connection__Download.DownloadOrderAbstract.LatestALS => {
        // Web doesn't support LatestALS (native binaries)
        None
      }
    | Connection__Download.DownloadOrderAbstract.DevALS => {
        // Web: Only check for WASM, ignore native binaries
        let wasmUri = VSCode.Uri.joinPath(globalStorageUri, ["dev-als", "als.wasm"])
        switch await FS.stat(wasmUri) {
        | Ok(_) => Some(VSCode.Uri.toString(wasmUri)) // Use URI string for WASM
        | Error(_) => None
        }
      }
    }
  }

  let resolveDownloadOrder = (
    order: Connection__Download.DownloadOrderAbstract.t,
    _useCache,
  ) => async (_memento, _globalStorageUri, _platform) => {
    switch order {
    | LatestALS => {
        // Web doesn't support LatestALS (native binaries)
        Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      }
    | DevALS => {
        // For DevALS on web, we download from UNPKG directly (not GitHub)
        // Return a mock descriptor so the UI shows the download option
        // The actual download uses the UNPKG path in Connection.res and State__SwitchVersion.res

        // Create mock release and asset for UI display
        // Use a name that matches the pattern expected by toVersionString: als-dev-Agda-<version>-...
        let mockAsset: Connection__Download__GitHub.Asset.t = {
          url: "https://unpkg.com/agda-wasm@0.0.3-als.2.8.0/als/2.8.0/als.wasm",
          id: 0,
          node_id: "mock-web-dev-als",
          name: "als-dev-Agda-2.8.0-wasm",
          label: Some("WASM Language Server"),
          content_type: "application/wasm",
          state: "uploaded",
          size: 0,
          created_at: "2024-01-01T00:00:00Z",
          updated_at: "2024-01-01T00:00:00Z",
          browser_download_url: "https://unpkg.com/agda-wasm@0.0.3-als.2.8.0/als/2.8.0/als.wasm",
        }
        let mockRelease: Connection__Download__GitHub.Release.t = {
          url: "https://api.github.com/repos/agda/agda-language-server/releases/mock",
          assets_url: "https://api.github.com/repos/agda/agda-language-server/releases/mock/assets",
          upload_url: "https://uploads.github.com/repos/agda/agda-language-server/releases/mock/assets",
          html_url: "https://github.com/agda/agda-language-server/releases/tag/dev",
          id: 0,
          node_id: "mock-web-dev-release",
          tag_name: "dev",
          target_commitish: "master",
          name: "dev",
          draft: false,
          prerelease: true,
          created_at: "2024-01-01T00:00:00Z",
          published_at: "2024-01-01T00:00:00Z",
          assets: [mockAsset],
          tarball_url: "https://api.github.com/repos/agda/agda-language-server/tarball/dev",
          zipball_url: "https://api.github.com/repos/agda/agda-language-server/zipball/dev",
          body: Some("Mock release for web WASM download from UNPKG"),
        }

        Ok(
          Connection__Download.DownloadOrderConcrete.FromGitHub(
            DevALS,
            {
              Connection__Download__GitHub.DownloadDescriptor.release: mockRelease,
              asset: mockAsset,
              saveAsFileName: "dev-als",
            },
          ),
        )
      }
    }
  }

  let download = Connection__Download.download

  // Always download, because there are no alternatives in web environment
  let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.Yes)
}

// Create platform dependencies for web environment
let make = (): Platform.t => module(Web)

// this function is the entry point for the web extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
