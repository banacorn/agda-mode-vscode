module Platform = {
  type t =
    | Wasm
    | Ubuntu
    | MacOSArm64
    | MacOSX64
    | Windows

  let parse = tag =>
    switch tag {
    | "wasm" => Some(Wasm)
    | "ubuntu" => Some(Ubuntu)
    | "macos-arm64" => Some(MacOSArm64)
    | "macos-x64" => Some(MacOSX64)
    | "windows" => Some(Windows)
    | _ => None
    }

  let toAssetTag = platform =>
    switch platform {
    | Wasm => "wasm"
    | Ubuntu => "ubuntu"
    | MacOSArm64 => "macos-arm64"
    | MacOSX64 => "macos-x64"
    | Windows => "windows"
    }

  let isWasm = platform =>
    switch platform {
    | Wasm => true
    | _ => false
    }

  let matchesDownloadPlatform = (artifactPlatform, downloadPlatform) =>
    switch (artifactPlatform, downloadPlatform) {
    | (Windows, Connection__Download__Platform.Windows) => true
    | (Ubuntu, Connection__Download__Platform.Ubuntu) => true
    | (MacOSArm64, Connection__Download__Platform.MacOS_Arm) => true
    | (MacOSX64, Connection__Download__Platform.MacOS_Intel) => true
    | (Wasm, Connection__Download__Platform.Web) => true
    | _ => false
    }

  let fromDownloadPlatform = (downloadPlatform: Connection__Download__Platform.t): t =>
    switch downloadPlatform {
    | Connection__Download__Platform.Windows => Windows
    | Connection__Download__Platform.Ubuntu => Ubuntu
    | Connection__Download__Platform.MacOS_Arm => MacOSArm64
    | Connection__Download__Platform.MacOS_Intel => MacOSX64
    | Connection__Download__Platform.Web => Wasm
    }
}

type t = {
  releaseTag: string,
  agdaVersion: string,
  platform: Platform.t,
}

let extensionMatchesPlatform = (platform, extension) =>
  switch (platform, extension) {
  | (_, None) => true
  | (Platform.Wasm, Some("wasm")) => true
  | (Platform.Wasm, Some(_)) => false
  | (_, Some("zip")) => true
  | (_, Some(_)) => false
  }

let parseName = (raw: string): option<t> =>
  switch String.match(
    raw,
    %re("/^als-([A-Za-z0-9.]+)-Agda-([0-9]+(?:\.[0-9]+)*)-(wasm|ubuntu|macos-arm64|macos-x64|windows)(?:\.(wasm|zip))?$/"),
  ) {
  | Some([_, Some(releaseTag), Some(agdaVersion), Some(platformTag), extension]) =>
    switch Platform.parse(platformTag) {
    | Some(platform) if extensionMatchesPlatform(platform, extension) =>
      Some({releaseTag, agdaVersion, platform})
    | _ => None
    }
  | _ => None
  }

let cacheName = artifact =>
  "als-" ++
  artifact.releaseTag ++
  "-Agda-" ++ artifact.agdaVersion ++ "-" ++ Platform.toAssetTag(artifact.platform)

let directoryParts = artifact => ["releases", artifact.releaseTag, cacheName(artifact)]

let executableName = artifact => Platform.isWasm(artifact.platform) ? "als.wasm" : "als"

let versionLabel = releaseTag =>
  releaseTag->String.startsWith("v") ? releaseTag : "v" ++ releaseTag


let managedExecutableUri = (globalStorageUri: VSCode.Uri.t, artifact: t): VSCode.Uri.t => {
  VSCode.Uri.joinPath(globalStorageUri, Array.concat(directoryParts(artifact), [
    executableName(artifact),
  ]))
}
