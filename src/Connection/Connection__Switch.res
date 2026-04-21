module Candidate = Connection__Candidate
module ResolvedMetadata = Memento.ResolvedMetadata

module SwitchVersionManager = {
  type t = {
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
  }

  let inferCandidateKind = (raw: string) => {
    let (baseName, parentDirName, releaseDirName, isUnderManagedReleases) = switch Candidate.make(
      raw,
    ) {
    | Candidate.Command(command) => (command->String.toLowerCase, None, None, false)
    | Candidate.Resource(uri) =>
      let path = VSCode.Uri.path(uri)
      let parentPath = path->NodeJs.Path.dirname
      let releasePath = parentPath->NodeJs.Path.dirname
      let releasesPath = releasePath->NodeJs.Path.dirname
      (
        path->NodeJs.Path.basename->String.toLowerCase,
        Some(parentPath->NodeJs.Path.basename),
        Some(releasePath->NodeJs.Path.basename),
        releasesPath->NodeJs.Path.basename == "releases",
      )
    }
    let isWasm = baseName->String.endsWith(".wasm")
    let cleanName =
      baseName
      ->String.replace(".exe", "")
      ->String.replace(".cmd", "")
      ->String.replace(".bat", "")
      ->String.replace(".wasm", "")

    if cleanName == "agda" || cleanName->String.startsWith("agda-") {
      ResolvedMetadata.Agda(None)
    } else if cleanName == "als" || cleanName->String.startsWith("als-") {
      switch parentDirName {
      | Some(dirName) =>
        switch Connection__Download.DownloadArtifact.parseName(dirName) {
        | Some(artifact) =>
          switch releaseDirName {
          | Some(releaseDirName)
              if isUnderManagedReleases && releaseDirName != artifact.releaseTag =>
            ResolvedMetadata.Unknown
          | _ =>
            ResolvedMetadata.ALS(
              isWasm ? WASM : Native,
              Some((artifact.releaseTag, artifact.agdaVersion, None)),
            )
          }
        | None =>
          isUnderManagedReleases
            ? ResolvedMetadata.Unknown
            : ResolvedMetadata.ALS(isWasm ? WASM : Native, None)
        }
      | _ => ResolvedMetadata.ALS(isWasm ? WASM : Native, None)
      }
    } else {
      ResolvedMetadata.Unknown
    }
  }

  let make = (state: State.t): t => {
    memento: state.memento,
    globalStorageUri: state.globalStorageUri,
  }

  let getItemData = async (
    self: t,
    downloadItems: array<(bool, string, string)>,
    ~downloadHeader: string="Download (Development)",
    ~platformDeps: option<Platform.t>=None,
  ): array<Connection__UI__ItemData.t> => {
    let storedPath = Memento.PreferredCandidate.get(self.memento)

    let pickedPath = switch storedPath {
    | Some(path) => Some(path)
    | None =>
      switch Registry__Connection.status.contents {
      | Active(resource) => Some(Connection.getPath(resource.connection))
      | _ => None
      }
    }

    let detailForCandidate = async (path: string, candidate: Candidate.t): string =>
      switch (candidate, platformDeps) {
      | (Candidate.Command(command), Some(platformDeps)) =>
        module PlatformOps = unpack(platformDeps)
        switch await Candidate.resolve(PlatformOps.findCommand, candidate) {
        | Ok(resolved) =>
          let resolvedPath =
            if VSCode.Uri.scheme(resolved.resource) == "file" {
              VSCode.Uri.fsPath(resolved.resource)
            } else {
              VSCode.Uri.toString(resolved.resource)
            }
          command ++ " (" ++ resolvedPath ++ ")"
        | Error(_) => command
        }
      | (Candidate.Command(command), None) => command
      | (Candidate.Resource(_), _) => path
      }

    let candidateEntries =
      await Promise.all(
        Config.Connection.getAgdaPaths()
        ->Array.toReversed
        ->Array.map(async path => {
          let candidate = Candidate.make(path)
          let detail = await detailForCandidate(path, candidate)
          let resolvedMetadata = switch (candidate, platformDeps) {
          | (Candidate.Resource(uri), _) =>
            let resolved: Candidate.Resolved.t = {original: candidate, resource: uri}
            Memento.ResolvedMetadata.get(self.memento, resolved)
          | (Candidate.Command(_), Some(platformDeps)) =>
            module PlatformOps = unpack(platformDeps)
            switch await Candidate.resolve(PlatformOps.findCommand, candidate) {
            | Ok(resolved) => Memento.ResolvedMetadata.get(self.memento, resolved)
            | Error(_) => None
            }
          | (Candidate.Command(_), None) => None
          }
          let entry = switch resolvedMetadata {
          | Some(entry) => entry
          | None => {
              kind: inferCandidateKind(path),
              timestamp: Date.make(),
              error: None,
            }
          }
          (path, detail, entry)
        }),
      )

    Connection__UI__ItemData.entriesToItemData(
      candidateEntries,
      pickedPath,
      downloadItems,
      ~downloadHeader,
    )
  }

  let probeVersions = async (self: t, platformDeps: Platform.t): bool => {
    let pathsToProbe = Config.Connection.getAgdaPaths()

    if Array.length(pathsToProbe) == 0 {
      false
    } else {
      module PlatformOps = unpack(platformDeps)
      let probePromises = pathsToProbe->Array.map(async path => {
        let candidate = Candidate.make(path)
        switch await Connection.probeCandidate(platformDeps, candidate) {
        | Ok((resolved, IsAgda(agdaVersion))) =>
          await Memento.ResolvedMetadata.setKind(
            self.memento,
            resolved,
            Agda(Some(agdaVersion)),
          )
          Some(path)
        | Ok((resolved, IsALS(alsVersion, agdaVersion, lspOptions))) =>
          await Memento.ResolvedMetadata.setKind(
            self.memento,
            resolved,
            ALS(Native, Some((alsVersion, agdaVersion, lspOptions))),
          )
          Some(path)
        | Ok((resolved, IsALSWASM(_))) =>
          let kind = switch Memento.ResolvedMetadata.get(self.memento, resolved) {
          | Some({kind: ResolvedMetadata.ALS(WASM, Some(versionInfo))}) =>
            ResolvedMetadata.ALS(WASM, Some(versionInfo))
          | _ =>
            switch inferCandidateKind(path) {
            | ResolvedMetadata.ALS(WASM, Some(versionInfo)) =>
              ResolvedMetadata.ALS(WASM, Some(versionInfo))
            | _ => ResolvedMetadata.ALS(WASM, None)
            }
          }
          await Memento.ResolvedMetadata.setKind(self.memento, resolved, kind)
          Some(path)
        | Error(error) =>
          let (_, errorBody) = Connection__Error.toString(Establish(error))
          switch await Candidate.resolve(PlatformOps.findCommand, candidate) {
          | Ok(resolved) =>
            await Memento.ResolvedMetadata.setError(self.memento, resolved, errorBody)
          | Error(_) => ()
          }
          Some(path)
        }
      })

      let updateResults = await Promise.all(probePromises)
      let updatedPaths = updateResults->Array.filterMap(x => x)

      Array.length(updatedPaths) > 0
    }
  }
}

// Connection switching logic
let switchAgdaVersion = async (state: State.t, selectedPath: string) => {
  let resolvedFromConnectionPath = (path: string): Connection__Candidate.Resolved.t => ({
    original: Candidate.make(selectedPath),
    resource: switch Connection__URI.parse(path) {
    | FileURI(_, uri) => uri
    },
  })

  await State__View.Panel.displayConnectionStatus(state, None)
  await State__View.Panel.display(
    state,
    AgdaModeVscode.View.Header.Plain("Switching connection..."),
    [],
  )

  switch await Connection.fromPathsOrCommands(
    state.platformDeps,
    [(selectedPath, Connection.Error.Establish.FromConfig)],
  ) {
  | Ok(conn) =>
    Util.log("[ debug ] switchAgdaVersion: connection succeeded", Connection.toString(conn))
    await Registry__Connection.shutdown()

    await Memento.PreferredCandidate.set(state.memento, Some(selectedPath))

    await State__View.Panel.displayConnectionStatus(state, Some(conn))
    await State__View.Panel.display(
      state,
      AgdaModeVscode.View.Header.Success("Switched to " ++ Connection.toString(conn)),
      [],
    )
    switch conn {
    | Agda(_, path, version) =>
      let resolved = resolvedFromConnectionPath(path)
      await Memento.ResolvedMetadata.setKind(
        state.memento,
        resolved,
        ResolvedMetadata.Agda(Some(version)),
      )
    | ALS(_, path, {alsVersion: Some(v), agdaVersion, lspOptions}) =>
      let resolved = resolvedFromConnectionPath(path)
      await Memento.ResolvedMetadata.setKind(
        state.memento,
        resolved,
        ResolvedMetadata.ALS(Native, Some(v, agdaVersion, lspOptions)),
      )
    | ALS(_, _, {alsVersion: None}) => ()
    | ALSWASM(_, _, path, {alsVersion: Some(v), agdaVersion, lspOptions}) =>
      let resolved = resolvedFromConnectionPath(path)
      await Memento.ResolvedMetadata.setKind(
        state.memento,
        resolved,
        ResolvedMetadata.ALS(WASM, Some(v, agdaVersion, lspOptions)),
      )
    | ALSWASM(_, _, _, {alsVersion: None}) => ()
    }
    let _ = await Connection.destroy(Some(conn), state.channels.log)
  | Error(error) => {
      let (errorHeader, errorBody) = Connection.Error.toString(Establish(error))
      Util.log("[ debug ] switchAgdaVersion: connection failed", errorHeader ++ " | " ++ errorBody)
      let header = AgdaModeVscode.View.Header.Error(
        "Failed to switch to a different installation: " ++ errorHeader,
      )
      let body = [AgdaModeVscode.Item.plainText(errorBody)]
      await State__View.Panel.display(state, header, body)
    }
  }
}

// Download module - handles download-related business logic
module Download = {
  type variant =
    | Native
    | WASM

  let variantToTag = variant =>
    switch variant {
    | Native => "native"
    | WASM => "wasm"
    }

  let variantFromTag = tag =>
    switch tag {
    | "native" => Some(Native)
    | "wasm" => Some(WASM)
    | _ => None
    }

  let variantDisplayName = variant =>
    switch variant {
    | Native => "Agda Language Server (native)"
    | WASM => "Agda Language Server (WASM)"
    }

  let variantFileName = variant =>
    switch variant {
    | Native => "als"
    | WASM => "als.wasm"
    }

  let unavailableItem = (variant: variant): (bool, string, string) => (
    false,
    Connection__UI__ItemData.Constants.downloadUnavailable,
    variantToTag(variant),
  )

  let getPlaceholderDownloadItems = async (platformDeps: Platform.t): array<(
    bool,
    string,
    string,
  )> => {
    module PlatformOps = unpack(platformDeps)
    switch await PlatformOps.determinePlatform() {
    | Ok(Connection__Download__Platform.Web) => [
        (false, Connection__UI__ItemData.Constants.checkingAvailability, variantToTag(WASM)),
      ]
    | _ => [
        (false, Connection__UI__ItemData.Constants.checkingAvailability, variantToTag(Native)),
        (false, Connection__UI__ItemData.Constants.checkingAvailability, variantToTag(WASM)),
      ]
    }
  }

  let getAllAvailableDownloads = async (
    state: State.t,
    platformDeps: Platform.t,
    ~channel: Connection__Download.Channel.t=DevALS,
  ): array<(bool, string, string)> =>
    await Connection__Download__Availability.getAll(
      state.memento,
      state.globalStorageUri,
      platformDeps,
      Config.Connection.getAgdaPaths(),
      ~channel,
      ~downloadUnavailable=Connection__UI__ItemData.Constants.downloadUnavailable,
    )
}

let activate = async (
  state: State.t,
  platformDeps: Platform.t,
  ~downloadItemsPromiseOverride: option<promise<array<(bool, string, string)>>>=None,
) => {
  let manager = SwitchVersionManager.make(state)
  await Connection__UI__Handlers.onActivate(
    state,
    platformDeps,
    ~getPlaceholderDownloadItems=() => Download.getPlaceholderDownloadItems(platformDeps),
    ~getDownloadItems=channel => Download.getAllAvailableDownloads(state, platformDeps, ~channel),
    ~getItemData=(downloadItems, ~downloadHeader) =>
      SwitchVersionManager.getItemData(
        manager,
        downloadItems,
        ~downloadHeader,
        ~platformDeps=Some(platformDeps),
      ),
    ~probeVersions=() => SwitchVersionManager.probeVersions(manager, platformDeps),
    ~hasSelectionChanged=selectedPath =>
      switch Memento.PreferredCandidate.get(manager.memento) {
      | Some(path) => !Candidate.equal(Candidate.make(selectedPath), Candidate.make(path))
      | None => true
      },
    ~switchCandidate=selectedPath => switchAgdaVersion(state, selectedPath),
    ~downloadItemsPromiseOverride,
  )
}
