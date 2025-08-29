// Mock implementations for testing to avoid dialogs and external dependencies

module DownloadDescriptor = {
  // Create a mock download descriptor that will make Connection.fromDownloads work
  let mockAsset = {
    Connection__Download__GitHub.Asset.url: "https://mock.url/download.zip",
    id: 123456,
    node_id: "mock_node_id",
    name: "als-Agda-2.6.3-Windows-x64.zip",
    label: "",
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

  let mockWith = (
    f: Connection__Download.target => result<
      Connection__Download__GitHub.DownloadDescriptor.t,
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
    let getDownloadDescriptor = DownloadDescriptor.mockWith(_ => Error(
      Connection__Download.Error.CannotFindCompatibleALSRelease,
    ))
    let getReleaseManifestFromGitHub = (_memento, _repo, ~useCache as _=true) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getDownloadDescriptorOfDevALS = (_memento, _globalStorageUri, _platform) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getDownloadDescriptorOfDevWASMALS = (_memento, _globalStorageUri, _) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getDownloadDescriptorOfLatestALS = (_memento, _globalStorageUri, _platform) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let download = (_globalStorageUri, _downloadDescriptor) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let findCommand = (_command, ~timeout as _timeout=1000) =>
      Promise.resolve(Error(Connection__Command.Error.NotFound))
    let openFolder = _uri => Promise.resolve()
  }

  // Mock platform that simulates successful Agda discovery
  module WithAgda = {
    let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
    let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No

    let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
    let getDownloadDescriptor = DownloadDescriptor.mockWith(_ => Error(
      Connection__Download.Error.CannotFindCompatibleALSRelease,
    ))
    let getReleaseManifestFromGitHub = (_memento, _repo, ~useCache as _=true) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getDownloadDescriptorOfDevALS = (_memento, _globalStorageUri, _platform) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getDownloadDescriptorOfDevWASMALS = (_memento, _globalStorageUri, _) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getDownloadDescriptorOfLatestALS = (_memento, _globalStorageUri, _platform) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let download = (_globalStorageUri, _downloadDescriptor) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let findCommand = (_command, ~timeout as _timeout=1000) =>
      Promise.resolve(Error(Connection__Command.Error.NotFound))
    let openFolder = _uri => Promise.resolve()
  }

  // Mock platform that allows download policy to be specified
  let makeWithDownloadPolicy = (policy: Config.Connection.DownloadPolicy.t): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => policy
      let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
      let getDownloadDescriptor = DownloadDescriptor.mockWith(_ => Error(
        Connection__Download.Error.CannotFindCompatibleALSRelease,
      ))
      let download = (_globalStorageUri, _downloadDescriptor) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      let openFolder = _uri => Promise.resolve()
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
      let getDownloadDescriptor = DownloadDescriptor.mockWith(_ => Error(
        Connection__Download.Error.CannotFindCompatibleALSRelease,
      ))
      let download = (_globalStorageUri, _downloadDescriptor) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      let openFolder = _uri => Promise.resolve()
    }
    module(MockPlatform)
  }

  // Mock platform that simulates successful download
  let makeWithSuccessfulDownload = (downloadedPath: string): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes

      let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
      let getDownloadDescriptor = DownloadDescriptor.mockWith(target =>
        switch target {
        | LatestALS => Ok(DownloadDescriptor.mockLatestALS)
        | DevALS => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        | DevWASMALS => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        }
      )
      let download = (_globalStorageUri, _downloadDescriptor) => Promise.resolve(Ok(downloadedPath))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      let openFolder = _uri => Promise.resolve()
    }
    module(MockPlatform)
  }

  // Mock platform that returns platform error
  let makeWithPlatformError = (platform): Platform.t => {
    module MockPlatform = {
      let determinePlatform = () => Promise.resolve(Error(platform))
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No

      let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
      let getDownloadDescriptor = DownloadDescriptor.mockWith(_ => Error(
        Connection__Download.Error.CannotFindCompatibleALSRelease,
      ))
      let download = (_globalStorageUri, _downloadDescriptor) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      let openFolder = _uri => Promise.resolve()
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
      let getDownloadDescriptor = DownloadDescriptor.mockWith(target =>
        switch target {
        | LatestALS => Ok(DownloadDescriptor.mockLatestALS)
        | DevALS => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        | DevWASMALS => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        }
      )
      let download = (_globalStorageUri, _downloadDescriptor) => {
        checkedDownloadFlag := true
        Promise.resolve(Ok(downloadedPath))
      }
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      let openFolder = _uri => Promise.resolve()
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

      let getDownloadDescriptor = DownloadDescriptor.mockWith(_ => Error(
        Connection__Download.Error.CannotFindCompatibleALSRelease,
      ))
      let download = (_globalStorageUri, _downloadDescriptor) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      let openFolder = _uri => Promise.resolve()
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
      let getDownloadDescriptor = DownloadDescriptor.mockWith(target =>
        switch target {
        | LatestALS => Ok(DownloadDescriptor.mockLatestALS)
        | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        }
      )
      let download = (_globalStorageUri, _downloadDescriptor) => {
        checkedDownloadFlag := true
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      }
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      let openFolder = _uri => Promise.resolve()
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

    let mockUri = VSCode.Uri.file("/test/path")

    State.make(platformDeps, channels, mockUri, mockUri, Memento.make(None), mockEditor, None)
  }
}
