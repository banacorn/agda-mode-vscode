// // Mock implementations for testing to avoid dialogs and external dependencies

// module Platform = {
//   // Basic mock platform that avoids all external interactions
//   module Basic = {
//     let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
//     let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
//     let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
//     let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
//       Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
//     let findCommand = (_command, ~timeout as _timeout=1000) =>
//       Promise.resolve(Error(Connection__Command.Error.NotFound))
//   }

//   // Mock platform that simulates successful Agda discovery
//   module WithAgda = {
//     let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
//     let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
//     let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
//     let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
//       Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
//     let findCommand = (_command, ~timeout as _timeout=1000) =>
//       Promise.resolve(Error(Connection__Command.Error.NotFound))
//   }

//   // Mock platform that allows download policy to be specified
//   let makeWithDownloadPolicy = (policy: Config.Connection.DownloadPolicy.t): Platform.t => {
//     module MockPlatform = {
//       let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
//       let askUserAboutDownloadPolicy = async () => policy
//       let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

//       let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
//         Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

//         let findCommand = (_command, ~timeout as _timeout=1000) =>
//         Promise.resolve(Error(Connection__Command.Error.NotFound))
//     }
//     module(MockPlatform)
//   }

//   // Mock platform that tracks download policy call count
//   let makeWithDownloadPolicyCounter = (
//     policy: Config.Connection.DownloadPolicy.t,
//     counter: ref<int>,
//   ): Platform.t => {
//     module MockPlatform = {
//       let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
//       let askUserAboutDownloadPolicy = async () => {
//         counter := counter.contents + 1
//         policy
//       }
//       let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

//       let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
//         Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

//         let findCommand = (_command, ~timeout as _timeout=1000) =>
//         Promise.resolve(Error(Connection__Command.Error.NotFound))
//     }
//     module(MockPlatform)
//   }

//   // Mock platform that simulates successful download
//   let makeWithSuccessfulDownload = (downloadedPath: string): Platform.t => {
//     module MockPlatform = {
//       let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
//       let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
//       let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

//       let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
//         Promise.resolve(Ok(downloadedPath))

//         let findCommand = (_command, ~timeout as _timeout=1000) =>
//         Promise.resolve(Error(Connection__Command.Error.NotFound))
//     }
//     module(MockPlatform)
//   }

//   // Mock platform that simulates cached download available
//   let makeWithCachedDownload = (cachedPath: string): Platform.t => {
//     module MockPlatform = {
//       let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
//       let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
//       let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(Some(cachedPath))

//       let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
//         Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

//         let findCommand = (_command, ~timeout as _timeout=1000) =>
//         Promise.resolve(Error(Connection__Command.Error.NotFound))
//     }
//     module(MockPlatform)
//   }

//   // Create basic mock platform that avoids dialogs
//   let makeBasic = (): Platform.t => module(Basic)

//   // Mock platform that finds agda in PATH and replicates Desktop.res logic
//   module WithAgdaInPath = {
//     let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
//     let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
//     let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
//     let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
//       Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
    
//     // Mock findCommand to return agda when searched (the key difference)
//     let findCommand = (command, ~timeout as _timeout=1000) => {
//       if command == "agda" {
//         Promise.resolve(Ok("/usr/bin/agda"))
//       } else {
//         Promise.resolve(Error(Connection__Command.Error.NotFound))
//       }
//     }
    
//   }

//   // Create mock platform with Agda available
//   let makeWithAgda = (): Platform.t => module(WithAgda)
  
//   // Create mock platform with Agda in PATH (for testing the real bug)
//   let makeWithAgdaInPath = (): Platform.t => module(WithAgdaInPath)
// }

// module Channels = {
//   // Create mock channels for testing
//   let make = () => {
//     State.inputMethod: Chan.make(),
//     responseHandled: Chan.make(),
//     commandHandled: Chan.make(),
//     log: Chan.make(),
//   }
// }

// module State = {
//   // Create a basic mock state for testing
//   let make = (platformDeps, channels: State.channels) => {
//     let mockEditor = %raw(`{
//       document: { fileName: "test.agda" }
//     }`)

//     let mockUri = VSCode.Uri.file("/test/path")

//     State.make(platformDeps, channels, mockUri, mockUri, None, mockEditor, None)
//   }
// }
