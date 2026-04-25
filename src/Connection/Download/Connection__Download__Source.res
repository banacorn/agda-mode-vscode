type t =
  | FromGitHub(Connection__Download__Channel.t, Connection__Download__GitHub.DownloadDescriptor.t)
  | FromURL(Connection__Download__Channel.t, string, string) // (channel, url, saveAsFileName)

let toString = source =>
  switch source {
  | FromGitHub(abstractChannel, descriptor) =>
    Connection__Download__Channel.toDisplayString(abstractChannel) ++
    Connection__Download__GitHub.DownloadDescriptor.toString(descriptor)
  | FromURL(abstractChannel, url, _) =>
    Connection__Download__Channel.toDisplayString(abstractChannel) ++ " from " ++ url
  }

let toVersionString = source =>
  switch source {
  | FromGitHub(abstractChannel, descriptor) =>
    switch Connection__Download__DownloadArtifact.parseName(descriptor.asset.name) {
    | Some(artifact) =>
      Connection__Download__Channel.artifactVersionString(
        ~channel=abstractChannel,
        artifact,
      )
    | None =>
      let agdaVersion = switch abstractChannel {
      | Connection__Download__Channel.LatestALS =>
        descriptor.asset.name
        ->String.replaceRegExp(%re("/als-Agda-/"), "")
        ->String.replaceRegExp(%re("/-.*/"), "")
      | Connection__Download__Channel.DevALS =>
        descriptor.asset.name
        ->String.replaceRegExp(%re("/als-dev-Agda-/"), "")
        ->String.replaceRegExp(%re("/-.*/"), "")
      }
      switch abstractChannel {
      | Connection__Download__Channel.LatestALS =>
        let alsVersion =
          descriptor.release.name
          ->String.split(".")
          ->Array.last
          ->Option.getOr(descriptor.release.name)
        "Agda v" ++ agdaVersion ++ " Language Server " ++ Connection__Download__DownloadArtifact.versionLabel(
          alsVersion,
        )
      | Connection__Download__Channel.DevALS =>
        "Agda v" ++ agdaVersion ++ " Language Server (dev build)"
      }
    }
  | FromURL(abstractChannel, _, _) =>
    Connection__Download__Channel.toDisplayString(abstractChannel)
  }
