// Mock implementations for testing to avoid dialogs and external dependencies

module Platform = {
  // Basic mock platform that avoids all external interactions
  module Basic = {
    let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
    let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No

    let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
    let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let findCommand = (_command, ~timeout as _timeout=1000) =>
      Promise.resolve(Error(Connection__Command.Error.NotFound))
    let openFolder = _uri => Promise.resolve()
  }

  // Mock platform that simulates successful Agda discovery
  module WithAgda = {
    let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
    let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No

    let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
    let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
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
      let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
      let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
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
      let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

      let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

      let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
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

      let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

      let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
        Promise.resolve(Ok(downloadedPath))

      let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
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

      let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

      let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

      let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
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

      let alreadyDownloaded = _globalStorageUri => () => {
        checkedCacheFlag := true
        Promise.resolve(None)
      }
      let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform => {
        checkedDownloadFlag := true
        Promise.resolve(Ok(downloadedPath))
      }

      let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
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
      let alreadyDownloaded = _globalStorageUri => () => {
        checkedFlag := true
        Promise.resolve(Some(cachedPath))
      }

      let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

      let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
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

      let alreadyDownloaded = _globalStorageUri => () => {
        checkedCacheFlag := true
        Promise.resolve(None)
      }
      let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform => {
        checkedDownloadFlag := true
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      }

      let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      let openFolder = _uri => Promise.resolve()
    }
    module(MockPlatform)
  }

  // Mock platform that simulates successful download with dual flag tracking
  let makeWithSuccessfulDownload2AndFlags = (
    downloadedPath: string,
    checkedCacheFlag: ref<bool>,
    checkedDownloadFlag: ref<bool>,
  ): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes

      let alreadyDownloaded = _globalStorageUri => () => {
        checkedCacheFlag := true
        Promise.resolve(None)
      }
      let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform => {
        checkedDownloadFlag := true
        Promise.resolve(Ok(downloadedPath))
      }

      let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
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

    State.make(platformDeps, channels, mockUri, mockUri, None, mockEditor, None)
  }
}
