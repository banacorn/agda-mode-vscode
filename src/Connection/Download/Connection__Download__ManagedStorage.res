module type Api = {
  let findAnyWasmDownloaded: VSCode.Uri.t => promise<option<string>>

  let findAnyDownloadedForPlatform: (
    VSCode.Uri.t,
    Connection__Download__Platform.t,
  ) => promise<option<string>>

  let findCandidateForSelection: (
    VSCode.Uri.t,
    ~channel: Connection__Download__Channel.t,
    ~platform: Connection__Download__DownloadArtifact.Platform.t,
    ~versionString: string,
  ) => promise<option<string>>

  let findWasmForManagedPath: (VSCode.Uri.t, string) => promise<option<string>>
}

module Internal: Api = {
let managedArtifactForPath = (
  globalStorageUri: VSCode.Uri.t,
  path: string,
): option<Connection__Download__DownloadArtifact.t> => {
  let storagePath = VSCode.Uri.fsPath(globalStorageUri)
  let rawPath = switch Connection__URI.parse(path) {
  | Connection__URI.FileURI(_, uri) => VSCode.Uri.fsPath(uri)
  }
  let relative = NodeJs.Path.relative(~from=storagePath, ~to_=rawPath)
  let parts = relative->String.splitByRegExp(%re("/[\\\\/]+/"))->Array.filterMap(x => x)

  switch Array.length(parts) == 4 {
  | true if Belt.Array.getExn(parts, 0) == "releases" =>
    let releaseName = Belt.Array.getExn(parts, 1)
    let artifactName = Belt.Array.getExn(parts, 2)
    let executableName = Belt.Array.getExn(parts, 3)
    switch Connection__Download__DownloadArtifact.parseName(artifactName) {
    | Some(artifact)
        if artifact.releaseTag == releaseName &&
          Connection__Download__DownloadArtifact.executableName(artifact) == executableName =>
      Some(artifact)
    | _ => None
    }
  | _ => None
  }
}

let findWasmForArtifact = async (
  globalStorageUri: VSCode.Uri.t,
  artifact: Connection__Download__DownloadArtifact.t,
): option<string> => {
  let wasmArtifact: Connection__Download__DownloadArtifact.t = {
    releaseTag: artifact.releaseTag,
    agdaVersion: artifact.agdaVersion,
    platform: Connection__Download__DownloadArtifact.Platform.Wasm,
  }
  let wasmUri = Connection__Download__DownloadArtifact.managedExecutableUri(globalStorageUri, wasmArtifact)
  switch await FS.stat(wasmUri) {
  | Ok(_) => Some(VSCode.Uri.toString(wasmUri))
  | Error(_) => None
  }
}

let findWasmForManagedPath = async (
  globalStorageUri: VSCode.Uri.t,
  path: string,
): option<string> =>
  switch managedArtifactForPath(globalStorageUri, path) {
  | Some(artifact) if !Connection__Download__DownloadArtifact.Platform.isWasm(artifact.platform) =>
    await findWasmForArtifact(globalStorageUri, artifact)
  | _ => None
  }

let findReleaseManagedDownloaded = async (
  globalStorageUri: VSCode.Uri.t,
  acceptArtifact,
  toPath,
): option<string> => {
  let releasesUri = VSCode.Uri.joinPath(globalStorageUri, ["releases"])

  let rec findInArtifacts = async (releaseName, artifacts, i) => {
    if i >= Array.length(artifacts) {
      None
    } else {
      let (artifactName, fileType) = Belt.Array.getExn(artifacts, i)
      if fileType != VSCode.FileType.Directory {
        await findInArtifacts(releaseName, artifacts, i + 1)
      } else {
        switch Connection__Download__DownloadArtifact.parseName(artifactName) {
        | Some(artifact) if artifact.releaseTag == releaseName && acceptArtifact(artifact) =>
          let executableUri = VSCode.Uri.joinPath(releasesUri, [
            releaseName,
            artifactName,
            Connection__Download__DownloadArtifact.executableName(artifact),
          ])
          switch await FS.stat(executableUri) {
          | Ok(_) => Some(toPath(executableUri))
          | Error(_) => await findInArtifacts(releaseName, artifacts, i + 1)
          }
        | _ => await findInArtifacts(releaseName, artifacts, i + 1)
        }
      }
    }
  }

  let rec findInReleases = async (releases, i) => {
    if i >= Array.length(releases) {
      None
    } else {
      let (releaseName, fileType) = Belt.Array.getExn(releases, i)
      if fileType != VSCode.FileType.Directory {
        await findInReleases(releases, i + 1)
      } else {
        let releaseUri = VSCode.Uri.joinPath(releasesUri, [releaseName])
        switch await FS.readDirectory(releaseUri) {
        | Error(_) => await findInReleases(releases, i + 1)
        | Ok(artifacts) => switch await findInArtifacts(releaseName, Connection__Download.sortDirectoryEntries(artifacts), 0) {
          | Some(path) => Some(path)
          | None => await findInReleases(releases, i + 1)
          }
        }
      }
    }
  }

  switch await FS.readDirectory(releasesUri) {
  | Error(_) => None
  | Ok(releases) => await findInReleases(Connection__Download.sortDirectoryEntries(releases), 0)
  }
}

let findAnyWasmDownloaded: VSCode.Uri.t => promise<option<string>> = (globalStorageUri) =>
  findReleaseManagedDownloaded(
    globalStorageUri,
    artifact => Connection__Download__DownloadArtifact.Platform.isWasm(artifact.platform),
    uri => VSCode.Uri.toString(uri),
  )

let findAnyDownloadedForPlatform: (
  VSCode.Uri.t,
  Connection__Download__Platform.t,
) => promise<option<string>> = async (globalStorageUri, platform) => {
  switch platform {
  | Web =>
    await findReleaseManagedDownloaded(
      globalStorageUri,
      artifact => Connection__Download__DownloadArtifact.Platform.isWasm(artifact.platform),
      uri => VSCode.Uri.toString(uri),
    )
  | _ =>
    switch await findReleaseManagedDownloaded(
      globalStorageUri,
      artifact =>
        Connection__Download__DownloadArtifact.Platform.matchesDownloadPlatform(
          artifact.platform,
          platform,
        ),
      uri => VSCode.Uri.fsPath(uri),
    ) {
    | Some(path) => Some(path)
    | None =>
      await findReleaseManagedDownloaded(
        globalStorageUri,
        artifact => Connection__Download__DownloadArtifact.Platform.isWasm(artifact.platform),
        uri => VSCode.Uri.toString(uri),
      )
    }
  }
}

let findCandidateForSelection: (
  VSCode.Uri.t,
  ~channel: Connection__Download__Channel.t,
  ~platform: Connection__Download__DownloadArtifact.Platform.t,
  ~versionString: string,
) => promise<option<string>> = async (globalStorageUri, ~channel, ~platform, ~versionString) => {
  await findReleaseManagedDownloaded(
    globalStorageUri,
    artifact =>
      Connection__Download__Channel.matchesArtifact(~channel, artifact) &&
      artifact.platform == platform &&
      Connection__Download__Channel.artifactVersionString(~channel, artifact) == versionString,
    Connection__Download.uriToPath,
  )
}
}

include (Internal: Api)
