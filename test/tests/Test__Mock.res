// Mock implementations for testing to avoid dialogs and external dependencies

module Platform = {
  // Basic mock platform that avoids all external interactions
  module Basic = {
    let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
    let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
    let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
    let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getInstalledEndpointsAndPersistThem = _globalStorageUri => Promise.resolve(Dict.make())
    let findCommand = (_command, ~timeout as _timeout=1000) =>
      Promise.resolve(Error(Connection__Command.Error.NotFound))
  }

  // Mock platform that simulates successful Agda discovery
  module WithAgda = {
    let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
    let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
    let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
    let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
      Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    let getInstalledEndpointsAndPersistThem = _globalStorageUri => {
      let endpoints = Dict.make()
      endpoints->Dict.set("/usr/bin/agda", Memento.Endpoints.Agda(Some("2.6.4")))
      Promise.resolve(endpoints)
    }
    let findCommand = (_command, ~timeout as _timeout=1000) =>
      Promise.resolve(Error(Connection__Command.Error.NotFound))
  }

  // Mock platform that allows download policy to be specified
  let makeWithDownloadPolicy = (policy: Config.Connection.DownloadPolicy.t): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => policy
      let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

      let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

      let getInstalledEndpointsAndPersistThem = _globalStorageUri => Promise.resolve(Dict.make())
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
      let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

      let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

      let getInstalledEndpointsAndPersistThem = _globalStorageUri => Promise.resolve(Dict.make())
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      
    }
    module(MockPlatform)
  }

  // Mock platform that simulates successful download
  let makeWithSuccessfulDownload = (downloadedEndpoint: Connection.Endpoint.t): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
      let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

      let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
        Promise.resolve(
          Ok(downloadedEndpoint->Connection__Endpoint.toURI->Connection__URI.toString),
        )

      let getInstalledEndpointsAndPersistThem = _globalStorageUri => Promise.resolve(Dict.make())
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      
    }
    module(MockPlatform)
  }

  // Mock platform that simulates cached download available
  let makeWithCachedDownload = (cachedEndpoint: Connection.Endpoint.t): Platform.t => {
    module MockPlatform = {
      let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
      let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
      let alreadyDownloaded = _globalStorageUri => () =>
        Promise.resolve(Some(cachedEndpoint->Connection__Endpoint.toURI->Connection__URI.toString))

      let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
        Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

      let getInstalledEndpointsAndPersistThem = _globalStorageUri => Promise.resolve(Dict.make())
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      
    }
    module(MockPlatform)
  }

  // Create basic mock platform that avoids dialogs
  let makeBasic = (): Platform.t => module(Basic)

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
