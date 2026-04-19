// Mock implementations for testing to avoid dialogs and external dependencies

module DownloadDescriptor = {
  // Create a mock download descriptor that will make Connection.fromDownloads work
  let mockAsset = {
    Connection__Download__GitHub.Asset.url: "https://mock.url/download.zip",
    id: 123456,
    node_id: "mock_node_id",
    name: "als-Agda-2.6.3-Windows-x64.zip",
    label: Some(""),
    content_type: "application/zip",
    state: "uploaded",
    size: 1000000,
    created_at: "2023-01-01T00:00:00Z",
    updated_at: "2023-01-01T00:00:00Z",
    browser_download_url: "https://mock.url/download.zip",
  }
  let mockRelease = {
    Connection__Download__GitHub.Release.url: "https://mock.url/release",
    assets_url: "https://mock.url/assets",
    upload_url: "https://mock.url/upload",
    html_url: "https://mock.url/html",
    id: 789012,
    node_id: "mock_release_node_id",
    tag_name: "v0.2.10",
    target_commitish: "main",
    name: "v0.2.10",
    draft: false,
    prerelease: false,
    created_at: "2023-01-01T00:00:00Z",
    published_at: "2023-01-01T00:00:00Z",
    assets: [mockAsset],
    tarball_url: "https://mock.url/tarball",
    zipball_url: "https://mock.url/zipball",
    body: Some("Mock release"),
  }
  let mockLatestALS = {
    Connection__Download__GitHub.DownloadDescriptor.asset: mockAsset,
    release: mockRelease,
    saveAsFileName: "als-Agda-2.6.3-Windows-x64",
  }

  // DevALS mock descriptor: asset name produces toVersionString "Agda v2.8.0 Language Server (dev build)"
  let mockDevALSDescriptorNativeAsset = {
    Connection__Download__GitHub.Asset.url: "https://github.com/agda/agda-language-server/releases/download/dev/als-dev-Agda-2.8.0-macos-arm64.zip",
    id: 0,
    node_id: "",
    name: "als-dev-Agda-2.8.0-macos-arm64.zip",
    label: Some(""),
    content_type: "application/zip",
    state: "uploaded",
    size: 1000000,
    created_at: "2024-01-01T00:00:00Z",
    updated_at: "2024-01-01T00:00:00Z",
    browser_download_url: "https://github.com/agda/agda-language-server/releases/download/dev/als-dev-Agda-2.8.0-macos-arm64.zip",
  }
  let mockDevALSDescriptorWasmAsset = {
    Connection__Download__GitHub.Asset.url: "https://github.com/agda/agda-language-server/releases/download/dev/als-dev-Agda-2.8.0-wasm.wasm",
    id: 1,
    node_id: "",
    name: "als-dev-Agda-2.8.0-wasm.wasm",
    label: Some(""),
    content_type: "application/octet-stream",
    state: "uploaded",
    size: 5000000,
    created_at: "2024-01-01T00:00:00Z",
    updated_at: "2024-01-01T00:00:00Z",
    browser_download_url: "https://github.com/agda/agda-language-server/releases/download/dev/als-dev-Agda-2.8.0-wasm.wasm",
  }
  let mockDevALSDescriptorRelease = {
    Connection__Download__GitHub.Release.url: "",
    assets_url: "",
    upload_url: "",
    html_url: "",
    id: 1,
    node_id: "dev",
    tag_name: "dev",
    target_commitish: "main",
    name: "dev",
    draft: false,
    prerelease: true,
    created_at: "2024-01-01T00:00:00Z",
    published_at: "2024-01-01T00:00:00Z",
    assets: [mockDevALSDescriptorNativeAsset, mockDevALSDescriptorWasmAsset],
    tarball_url: "",
    zipball_url: "",
    body: None,
  }
  let mockDevALSDescriptor = {
    Connection__Download__GitHub.DownloadDescriptor.asset: mockDevALSDescriptorNativeAsset,
    release: mockDevALSDescriptorRelease,
    saveAsFileName: "dev-als",
  }

  let mockWith = (
    f: Connection__Download.Channel.t => result<
      Connection__Download.Source.t,
      Connection__Download.Error.t,
    >,
  ) => (target, _) => async (_, _, _) => f(target)
}

module Platform = {
  // Basic mock platform that avoids all external interactions
  module Basic = {
    let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
    let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No

    let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
    let resolveDownloadChannel = DownloadDescriptor.mockWith(_ => Error(
      Connection__Download.Error.CannotFindCompatibleALSRelease,
    ))
    let getReleaseManifestFromGitHub = (_memento, _repo, ~useCache as _=true) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let resolveDownloadChannelOfDevALS = (_memento, _globalStorageUri, _platform) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let resolveDownloadChannelOfLatestALS = (_memento, _globalStorageUri, _platform) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let findCommand = (_command, ~timeout as _timeout=1000) =>
      Promise.resolve(Error(Connection__Command.Error.NotFound))
  }

  // Mock platform that simulates successful Agda discovery
  module WithAgda = {
    let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
    let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No

    let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
    let resolveDownloadChannel = DownloadDescriptor.mockWith(_ => Error(
      Connection__Download.Error.CannotFindCompatibleALSRelease,
    ))
    let getReleaseManifestFromGitHub = (_memento, _repo, ~useCache as _=true) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let resolveDownloadChannelOfDevALS = (_memento, _globalStorageUri, _platform) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let resolveDownloadChannelOfLatestALS = (_memento, _globalStorageUri, _platform) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let findCommand = (_command, ~timeout as _timeout=1000) =>
      Promise.resolve(Error(Connection__Command.Error.NotFound))
  }

  // Mock platform that allows download policy to be specified
  let makeWithDownloadPolicy = (policy: Config.Connection.DownloadPolicy.t): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => policy
      let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
      let resolveDownloadChannel = DownloadDescriptor.mockWith(_ => Error(
        Connection__Download.Error.CannotFindCompatibleALSRelease,
      ))
      let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
    }
    module(MockPlatform)
  }

  // Mock platform that tracks download policy call count
  let makeWithDownloadPolicyCounter = (
    policy: Config.Connection.DownloadPolicy.t,
    counter: ref<int>,
  ): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => {
        counter := counter.contents + 1
        policy
      }
      let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
      let resolveDownloadChannel = DownloadDescriptor.mockWith(_ => Error(
        Connection__Download.Error.CannotFindCompatibleALSRelease,
      ))
      let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
    }
    module(MockPlatform)
  }

  // Mock platform that simulates successful download
  let makeWithSuccessfulDownload = (downloadedPath: string): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes

      let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
      let resolveDownloadChannel = DownloadDescriptor.mockWith(channel =>
        switch channel {
        | Connection__Download.Channel.LatestALS =>
          Ok(FromGitHub(channel, DownloadDescriptor.mockLatestALS))
        | Connection__Download.Channel.DevALS =>
          Ok(FromGitHub(channel, DownloadDescriptor.mockDevALSDescriptor))
        }
      )
      let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) => Promise.resolve(Ok(downloadedPath))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
    }
    module(MockPlatform)
  }

  // Mock platform that returns platform error
  let makeWithPlatformError = (platform): Platform.t => {
    module MockPlatform = {
      let determinePlatform = () => Promise.resolve(Error(platform))
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No

      let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
      let resolveDownloadChannel = DownloadDescriptor.mockWith(_ => Error(
        Connection__Download.Error.CannotFindCompatibleALSRelease,
      ))
      let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
    }
    module(MockPlatform)
  }

  // Mock platform that simulates successful download with dual flag tracking
  let makeWithSuccessfulDownloadAndFlags = (
    downloadedPath: string,
    checkedCacheFlag: ref<bool>,
    checkedDownloadFlag: ref<bool>,
  ): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes

      let alreadyDownloaded = (_globalStorageUri, _) => {
        checkedCacheFlag := true
        Promise.resolve(None)
      }
      let resolveDownloadChannel = DownloadDescriptor.mockWith(channel =>
        switch channel {
        | Connection__Download.Channel.LatestALS =>
          Ok(FromGitHub(channel, DownloadDescriptor.mockLatestALS))
        | Connection__Download.Channel.DevALS =>
          Ok(FromURL(DevALS, "mock-url", "dev-als"))
        }
      )
      let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) => {
        checkedDownloadFlag := true
        Promise.resolve(Ok(downloadedPath))
      }
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
    }
    module(MockPlatform)
  }

  // Mock platform that simulates cached download available with flag tracking
  let makeWithCachedDownloadAndFlag = (cachedPath: string, checkedFlag: ref<bool>): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
      let alreadyDownloaded = (_globalStorageUri, _) => {
        checkedFlag := true
        Promise.resolve(Some(cachedPath))
      }

      let resolveDownloadChannel = DownloadDescriptor.mockWith(_ => Error(
        Connection__Download.Error.CannotFindCompatibleALSRelease,
      ))
      let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
    }
    module(MockPlatform)
  }

  // Create basic mock platform that avoids dialogs
  let makeBasic = (): Platform.t => module(Basic)

  // Mock platform that fails to download ALS with flag tracking
  let makeWithDownloadFailureAndFlags = (
    checkedCacheFlag: ref<bool>,
    checkedDownloadFlag: ref<bool>,
  ): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.Windows)
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes

      let alreadyDownloaded = (_globalStorageUri, _) => {
        checkedCacheFlag := true
        Promise.resolve(None)
      }
      let resolveDownloadChannel = DownloadDescriptor.mockWith(channel =>
        switch channel {
        | LatestALS => Ok(FromGitHub(channel, DownloadDescriptor.mockLatestALS))
        | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        }
      )
      let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) => {
        checkedDownloadFlag := true
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      }
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
    }
    module(MockPlatform)
  }

  // Mock platform that fails DevALS native download and succeeds on WASM fallback
  let makeWithNativeFailureAndWASMSuccess = (
    downloadedPath: string,
    checkedCacheFlag: ref<bool>,
    checkedNativeDownloadFlag: ref<bool>,
    checkedWasmDownloadFlag: ref<bool>,
  ): Platform.t => {
    let makeAsset = (name): Connection__Download__GitHub.Asset.t => {
      url: "https://github.com/agda/agda-language-server/releases/download/dev/" ++ name,
      id: 0,
      node_id: "",
      name,
      label: Some(""),
      content_type: "application/zip",
      state: "uploaded",
      size: 1000000,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      browser_download_url: "https://github.com/agda/agda-language-server/releases/download/dev/" ++ name,
    }
    let nativeAssetName = "als-dev-Agda-2.8.0-ubuntu.zip"
    let wasmAssetName = "als-dev-Agda-2.8.0-wasm.wasm"
    let devRelease: Connection__Download__GitHub.Release.t = {
      url: "", assets_url: "", upload_url: "", html_url: "",
      id: 1, node_id: "dev", tag_name: "dev", target_commitish: "main", name: "dev",
      draft: false, prerelease: true,
      created_at: "2024-01-01T00:00:00Z", published_at: "2024-01-01T00:00:00Z",
      assets: [makeAsset(nativeAssetName), makeAsset(wasmAssetName)],
      tarball_url: "", zipball_url: "", body: None,
    }
    let nativeDescriptor: Connection__Download__GitHub.DownloadDescriptor.t = {
      asset: makeAsset(nativeAssetName),
      release: devRelease,
      saveAsFileName: "dev-als",
    }

    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.Ubuntu)
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes

      let alreadyDownloaded = (_globalStorageUri, _) => {
        checkedCacheFlag := true
        Promise.resolve(None)
      }
      let resolveDownloadChannel = DownloadDescriptor.mockWith(channel =>
        switch channel {
        | Connection__Download.Channel.DevALS =>
          Ok(
            Connection__Download.Source.FromGitHub(
              Connection__Download.Channel.DevALS,
              nativeDescriptor,
            ),
          )
        | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        }
      )
      let download = (_globalStorageUri, source, ~trace as _=Connection__Download__Trace.noop) =>
        switch source {
        | Connection__Download.Source.FromGitHub(Connection__Download.Channel.DevALS, descriptor) =>
          if descriptor.asset.name->String.includes("wasm") {
            checkedWasmDownloadFlag := true
            Promise.resolve(Ok(downloadedPath))
          } else {
            checkedNativeDownloadFlag := true
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          }
        | _ =>
          Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
        }
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
    }
    module(MockPlatform)
  }

  // Create mock platform with Agda available
  let makeWithAgda = (): Platform.t => module(WithAgda)
}

module Channels = {
  // Create mock channels for testing
  let make = () => {
    State.inputMethod: Chan.make(),
    responseHandled: Chan.make(),
    commandHandled: Chan.make(),
    log: Chan.make(),
  }
}

module State = {
  // Create a basic mock state for testing
  let make = (platformDeps, channels: State.channels) => {
    let mockEditor = %raw(`{
      document: { fileName: "test.agda" }
    }`)

    let mockStorageUri = VSCode.Uri.file(NodeJs.Os.tmpdir())
    let mockExtensionUri = VSCode.Uri.file(NodeJs.Process.cwd(NodeJs.Process.process))

    State.make(
      "mock-id",
      platformDeps,
      channels,
      mockStorageUri,
      mockExtensionUri,
      Memento.make(None),
      mockEditor,
      None,
    )
  }
}
