// datatype for logging
module SwitchVersion = {
  type t =
    | Destroyed // when the SwitchVersion UI has been destroyed
    | SelectedCandidate(string, Memento.ResolvedMetadata.entry, bool)
    | SelectedDownloadAction(bool, string) // downloaded, versionString
    | SelectedOpenFolder(string)
    | SelectedNoInstallations
    | UpdatedCandidates(array<(string, Memento.ResolvedMetadata.kind, option<string>, bool)>) // array of (path, kind, optional error, isSelected)
    | UpdatedDownloadItems(array<(bool, string, string)>) // array of (downloaded, versionString, variant)
    | SelectionCompleted // when onSelection handler has completed all async operations
    | Others(string)

  let toString = event =>
    switch event {
    | Destroyed => "Destroyed"
    | SelectedCandidate(path, entry, isSelected) =>
      "Candidate: " ++
      path ++
      ", " ++
      Memento.ResolvedMetadata.kindToString(entry.kind) ++ if isSelected {
        ", selected"
      } else {
        ""
      }
    | SelectedDownloadAction(downloaded, versionString) =>
      "Selected Download Action: downloaded=" ++
      string_of_bool(downloaded) ++
      ", versionString=" ++
      versionString
    | SelectedOpenFolder(path) => "Selected Open Folder: " ++ path
    | SelectedNoInstallations => "Selected No Installations"
    | SelectionCompleted => "Selection Completed"
    | UpdatedCandidates(entries) =>
      "UpdatedCandidates: " ++
      entries
      ->Array.map(((path, kind, error, isSelected)) =>
        path ++
        ": " ++
        switch kind {
        | Agda(Some(version)) => "Agda(" ++ version ++ ")"
        | Agda(None) => "Agda(None)"
        | ALS(Native, Some((alsVersion, agdaVersion, _))) =>
          "ALS(Native, " ++ alsVersion ++ ", " ++ agdaVersion ++ ")"
        | ALS(WASM, Some((alsVersion, agdaVersion, _))) =>
          "ALS(WASM, " ++ alsVersion ++ ", " ++ agdaVersion ++ ")"
        | ALS(Native, None) => "ALS(Native, None)"
        | ALS(WASM, None) => "ALS(WASM, None)"
        | Unknown => "Unknown"
        } ++
        switch error {
        | Some(err) => " [Error: " ++ err ++ "]"
        | None => ""
        } ++ (isSelected ? " [Selected]" : "")
      )
      ->Array.join("\n")
    | UpdatedDownloadItems(items) =>
      "UpdatedDownloadItems: " ++
      items
      ->Array.map(((downloaded, vs, variant)) =>
        variant ++ "=" ++ vs ++ if downloaded { "(downloaded)" } else { "" }
      )
      ->Array.join(", ")
    | Others(str) => str
    }
}

module Connection = {
  module ActivationFlow = {
    type t =
      | ActivationStarted
      | ExistingConnectionReused
      | FreshEstablishStarted
      | ActivationSucceeded
      | ActivationFailed

    let toString = event =>
      switch event {
      | ActivationStarted => "ActivationStarted"
      | ExistingConnectionReused => "ExistingConnectionReused"
      | FreshEstablishStarted => "FreshEstablishStarted"
      | ActivationSucceeded => "ActivationSucceeded"
      | ActivationFailed => "ActivationFailed"
      }
  }

  module SwitchUIFlow = {
    type t =
      | SwitchRequested(string) // selectedPath
      | SwitchSucceeded(string) // finalPath
      | SwitchFailed(string) // selectedPath

    let toString = event =>
      switch event {
      | SwitchRequested(selectedPath) => "SwitchRequested: " ++ selectedPath
      | SwitchSucceeded(finalPath) => "SwitchSucceeded: " ++ finalPath
      | SwitchFailed(selectedPath) => "SwitchFailed: " ++ selectedPath
      }
  }

  module EstablishFlow = {
    type connectionKind = Agda | ALS | ALSWASM

    type t =
      | ConfigCandidatesPlanned(int)
      | CandidateAttempted(string, Connection__Error.Establish.pathSource)
      | ConfigCandidatesFailed
      | DownloadFallbackStarted(
          Connection__Download__Channel.t,
          Connection__Download__DownloadArtifact.Platform.t,
        )
      | DownloadFallbackFailed(Connection__Error.Establish.t)
      // Connection.t object created; outer flows (SwitchUIFlow, ActivationFlow) report operation success
      | ConnectionCreated(string, connectionKind)
      | ConnectionEstablishFailed
      // emitted from the switch path only: connection was created, but post-establish switch finalization failed
      | ConnectionFinalizeFailed(string, connectionKind)

    let toString = event =>
      switch event {
      | ConfigCandidatesPlanned(count) => "ConfigCandidatesPlanned: " ++ string_of_int(count)
      | CandidateAttempted(pathOrCommand, source) =>
        "CandidateAttempted: " ++
        pathOrCommand ++
        " (" ++
        Connection__Error.Establish.pathSourceToString(source) ++
        ")"
      | ConfigCandidatesFailed => "ConfigCandidatesFailed"
      | DownloadFallbackStarted(channel, platform) =>
        "DownloadFallbackStarted: channel=" ++
        Connection__Download__Channel.toString(channel) ++
        " platform=" ++
        Connection__Download__DownloadArtifact.Platform.toAssetTag(platform)
      | DownloadFallbackFailed(error) =>
        "DownloadFallbackFailed: " ++ Connection__Error.Establish.toString(error)
      | ConnectionCreated(path, Agda) => "ConnectionCreated: " ++ path ++ " (Agda)"
      | ConnectionCreated(path, ALS) => "ConnectionCreated: " ++ path ++ " (ALS)"
      | ConnectionCreated(path, ALSWASM) => "ConnectionCreated: " ++ path ++ " (ALSWASM)"
      | ConnectionEstablishFailed => "ConnectionEstablishFailed"
      | ConnectionFinalizeFailed(path, Agda) => "ConnectionFinalizeFailed: " ++ path ++ " (Agda)"
      | ConnectionFinalizeFailed(path, ALS) => "ConnectionFinalizeFailed: " ++ path ++ " (ALS)"
      | ConnectionFinalizeFailed(path, ALSWASM) =>
        "ConnectionFinalizeFailed: " ++ path ++ " (ALSWASM)"
      }
  }

  module ProbeFlow = {
    type t =
      | CandidateResolveStarted(Connection__Candidate.t)
      | CandidateResolved(Connection__Candidate.t, VSCode.Uri.t)
      | CandidateResolveFailed(Connection__Candidate.t, Connection__Command.Error.t)
      | ProbeStarted(VSCode.Uri.t)
      | ProbeClassifiedAsAgda(string, string) // path, agdaVersion
      | ProbeClassifiedAsALS(string, string, string) // path, alsVersion, agdaVersion
      | ProbeClassifiedAsALSWASM(string) // pathOrUri
      | ProbeFailed(string, Connection__Error.Probe.t) // pathKey, error

    let toString = event =>
      switch event {
      | CandidateResolveStarted(candidate) =>
        "CandidateResolveStarted: " ++ Connection__Candidate.toString(candidate)
      | CandidateResolved(original, resource) =>
        "CandidateResolved: " ++
        Connection__Candidate.toString(original) ++
        " -> " ++
        VSCode.Uri.toString(resource)
      | CandidateResolveFailed(original, commandError) =>
        "CandidateResolveFailed: " ++
        Connection__Candidate.toString(original) ++
        " -> " ++
        Connection__Command.Error.toString(commandError)
      | ProbeStarted(resource) => "ProbeStarted: " ++ VSCode.Uri.toString(resource)
      | ProbeClassifiedAsAgda(path, version) =>
        "ProbeClassifiedAsAgda: " ++ path ++ " (Agda v" ++ version ++ ")"
      | ProbeClassifiedAsALS(path, alsVersion, agdaVersion) =>
        "ProbeClassifiedAsALS: " ++
        path ++
        " (ALS v" ++
        alsVersion ++
        ", Agda v" ++
        agdaVersion ++
        ")"
      | ProbeClassifiedAsALSWASM(pathOrUri) => "ProbeClassifiedAsALSWASM: " ++ pathOrUri
      | ProbeFailed(pathKey, error) =>
        "ProbeFailed: " ++ pathKey ++ " -> " ++ Connection__Error.Probe.toString(error)
      }
  }

  module DownloadFlow = {
    type sourceKind = Managed | GitHub | URL

    type t =
      | SelectionRequested(
          Connection__Download__Channel.t,
          Connection__Download__DownloadArtifact.Platform.t,
          string,
          bool,
        ) // channel, platform, versionString, alreadyDownloaded
      | ManagedHit(string, string) // versionString, path
      | ManagedMiss(string) // versionString
      | SourceResolved(sourceKind, string) // sourceKind, versionString
      | ReusedExistingArtifact(string) // path
      | DownloadStarted(
          Connection__Download__Channel.t,
          Connection__Download__DownloadArtifact.Platform.t,
          string,
        ) // channel, platform, versionString
      | DownloadSucceeded(string) // path
      | DownloadFailed(string) // errorMsg
      | FallbackChosen(string) // versionString

    let toString = event =>
      switch event {
      | SelectionRequested(channel, platform, vs, downloaded) =>
        `SelectionRequested: channel=${Connection__Download__Channel.toString(channel)} platform=${Connection__Download__DownloadArtifact.Platform.toAssetTag(platform)} version=${vs} downloaded=${string_of_bool(downloaded)}`
      | ManagedHit(vs, path) => `ManagedHit: ${vs} at ${path}`
      | ManagedMiss(vs) => `ManagedMiss: ${vs}`
      | SourceResolved(Managed, vs) => `SourceResolved(Managed): ${vs}`
      | SourceResolved(GitHub, vs) => `SourceResolved(GitHub): ${vs}`
      | SourceResolved(URL, vs) => `SourceResolved(URL): ${vs}`
      | ReusedExistingArtifact(path) => `ReusedExistingArtifact: ${path}`
      | DownloadStarted(channel, platform, vs) =>
        `DownloadStarted: channel=${Connection__Download__Channel.toString(channel)} platform=${Connection__Download__DownloadArtifact.Platform.toAssetTag(platform)} version=${vs}`
      | DownloadSucceeded(path) => `DownloadSucceeded: ${path}`
      | DownloadFailed(msg) => `DownloadFailed: ${msg}`
      | FallbackChosen(vs) => `FallbackChosen: ${vs}`
      }
  }

  type t =
    | ActivationFlow(ActivationFlow.t) // top-level activation/acquisition flow
    | SwitchUIFlow(SwitchUIFlow.t) // top-level switch-ui intent/result flow
    | EstablishFlow(EstablishFlow.t) // top-level connection-establishment flow
    | ProbeFlow(ProbeFlow.t) // structured probe / candidate-resolution observability event
    | DownloadFlow(DownloadFlow.t) // structured download flow observability event
    | ConnectedToAgda(string, string) // path, version
    | ConnectedToALS(string, option<(string, string)>) // path, ALS version, Agda version
    | Disconnected(string) // path

  let toString = event =>
    switch event {
    | ActivationFlow(event) => "[ ActivationFlow   ] " ++ ActivationFlow.toString(event)
    | SwitchUIFlow(event) => "[ SwitchUIFlow    ] " ++ SwitchUIFlow.toString(event)
    | EstablishFlow(event) => "[ EstablishFlow    ] " ++ EstablishFlow.toString(event)
    | ProbeFlow(event) => "[ ProbeFlow        ] " ++ ProbeFlow.toString(event)
    | DownloadFlow(event) => "[ DownloadFlow     ] " ++ DownloadFlow.toString(event)
    | ConnectedToAgda(path, version) => `ConnectedToAgda: ${path} - Agda v${version}`
    | ConnectedToALS(path, Some(alsVersion, agdaVersion)) =>
      `ConnectedToALS: ${path} - Agda v${agdaVersion} Language Server v${alsVersion}`
    | ConnectedToALS(path, None) => `ConnectedToALS: ${path} -  Agda Language Server of unknown version`
    | Disconnected(path) => `Disconnected: ${path}`
    }
}

module Registry = {
  type t =
    | Lookup(string, bool) // filepath, found
    | Add(string) // filepath
    | Remove(string) // filepath

  let toString = event =>
    switch event {
    | Lookup(filepath, found) => "lookup: " ++ filepath ++ " " ++ (found ? "found" : "not found")
    | Add(filepath) => "add: " ++ filepath
    | Remove(filepath) => "remove: " ++ filepath
    }
}

module Config = {
  type t = Changed(array<string>, array<string>) // before, after

  let toString = event =>
    switch event {
    | Changed(before, after) =>
      "Config changed:\n" ++
      "Before: " ++
      Array.join(before, ", ") ++
      "\n" ++
      "After: " ++
      Array.join(after, ", ")
    }
}

module InputMethod = {
  module ApplyEditResult = {
    type t =
      | Succeeded
      | Failed
      | Threw(string)
  }

  type textChangeRouting = {
    capturedEditorDocumentFileName: string,
    stateEditorDocumentFileName: string,
    eventDocumentFileName: string,
    capturedEditorIsStateEditor: bool,
    capturedDocumentIsStateDocument: bool,
    eventDocumentIsStateDocument: bool,
    capturedDocumentIsEventDocument: bool,
  }

  type rewriteApplication = {
    targetDocumentFileName: string,
    replacementCount: int,
    applyEditResult: ApplyEditResult.t,
  }

  type t =
    | EngineLog(IM.Log.t)
    | TextChangeRouting(textChangeRouting)
    | RewriteApplication(rewriteApplication)

  let toString = event =>
    switch event {
    | EngineLog(kinds) => "EngineLog(" ++ string_of_int(Array.length(kinds)) ++ " events)"
    | TextChangeRouting(r) =>
      "TextChangeRouting: capturedEditor=" ++
      r.capturedEditorDocumentFileName ++
      " stateEditor=" ++
      r.stateEditorDocumentFileName ++
      " event=" ++
      r.eventDocumentFileName ++
      " capturedIsStateEditor=" ++
      string_of_bool(r.capturedEditorIsStateEditor) ++
      " capturedDocIsStateDoc=" ++
      string_of_bool(r.capturedDocumentIsStateDocument) ++
      " eventDocIsStateDoc=" ++
      string_of_bool(r.eventDocumentIsStateDocument) ++
      " capturedDocIsEventDoc=" ++
      string_of_bool(r.capturedDocumentIsEventDocument)
    | RewriteApplication(r) =>
      "RewriteApplication: target=" ++
      r.targetDocumentFileName ++
      " count=" ++
      string_of_int(r.replacementCount) ++
      " result=" ++
      switch r.applyEditResult {
      | Succeeded => "Succeeded"
      | Failed => "Failed"
      | Threw(msg) => "Threw(" ++ msg ++ ")"
      }
    }
}

type t =
  | CommandDispatched(Command.t)
  | CommandHandled(Command.t)
  | RequestSent(Request.t)
  | ResponseHandled(Response.t)
  | Registry(Registry.t)
  | TokensReset(string) // reason
  | SwitchVersionUI(SwitchVersion.t) // SwitchVersion UI event
  | Connection(Connection.t) // Connection event
  | Config(Config.t) // Configuration event
  | DownloadTrace(Connection__Download__Trace.t) // low-level download trace event
  | InputMethod(InputMethod.t) // input method diagnostics
  | Others(string) // generic string

let toString = log =>
  switch log {
  | CommandDispatched(command) => " <=== " ++ Command.toString(command)
  | RequestSent(request) => "   <- " ++ Request.toString(request)
  | ResponseHandled(response) => "    > " ++ Response.toString(response)
  | CommandHandled(command) => " ===> " ++ Command.toString(command)
  | Registry(event) => "[ Registry         ] " ++ Registry.toString(event)
  | TokensReset(reason) => "Tokens reset: " ++ reason
  | SwitchVersionUI(event) => "[ SwitchVersion    ] " ++ SwitchVersion.toString(event)
  | Connection(event) => "[ Connection       ] " ++ Connection.toString(event)
  | Config(event) => "[ Config           ] " ++ Config.toString(event)
  | DownloadTrace(event) => "[ DownloadTrace    ] " ++ Connection__Download__Trace.toString(event)
  | InputMethod(event) => "[ InputMethod      ] " ++ InputMethod.toString(event)
  | Others(str) => str
  }

let isConfig = log =>
  switch log {
  | Config(_) => true
  | _ => false
  }

let isConnection = log =>
  switch log {
  | Connection(Connection.ConnectedToAgda(_, _))
  | Connection(Connection.ConnectedToALS(_, _))
  | Connection(Connection.Disconnected(_)) => true
  | _ => false
  }

// collect logs from the log channel from the time of the call
// returns a function that returns the collected logs and stops collecting
let collect = (channel: Chan.t<t>): ((~filter: t => bool=?) => array<t>) => {
  let logs = []

  let handle = Chan.on(channel, log => {
    logs->Array.push(log)
  })

  (~filter=?) => {
    handle()
    switch filter {
    | Some(f) => logs->Array.filter(f)
    | None => logs
    }
  }
}

// awaits for the log channel to receive a certain log that satisfies the predicate
let on = async (channel: Chan.t<t>, predicate: t => bool): unit => {
  let (promise, resolve, _) = Util.Promise_.pending()

  let handle = Chan.on(channel, log => {
    if predicate(log) {
      resolve()
    }
  })
  await promise
  handle()
}
