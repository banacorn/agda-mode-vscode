// Hardcoded URLs for downloading Agda Language Server binaries
// These bypass GitHub API discovery and use direct download URLs

let alsVersion = "0.2.7.0.1.5"
let agdaVersion = "2.7.0.1"
let wasmAgdaVersion = "2.8.0"

let baseUrl = "https://github.com/agda/agda-language-server/releases/download/v" ++ alsVersion

let nativeUrlForPlatform = platform =>
  switch platform {
  | Connection__Download__Platform.Windows =>
    Some(baseUrl ++ "/als-Agda-" ++ agdaVersion ++ "-windows.zip")
  | Connection__Download__Platform.Ubuntu =>
    Some(baseUrl ++ "/als-Agda-" ++ agdaVersion ++ "-ubuntu.zip")
  | Connection__Download__Platform.MacOS_Arm =>
    Some(baseUrl ++ "/als-Agda-" ++ agdaVersion ++ "-macos-arm64.zip")
  | Connection__Download__Platform.MacOS_Intel =>
    Some(baseUrl ++ "/als-Agda-" ++ agdaVersion ++ "-macos-x64.zip")
  | Connection__Download__Platform.Web => None
  }

let wasmUrl = "https://unpkg.com/agda-wasm@0.0.3-als." ++ wasmAgdaVersion ++ "/als/" ++ wasmAgdaVersion ++ "/als.wasm"
