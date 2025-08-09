// datatype for logging
module SwitchVersion = {
  type t =
    | Destroyed // when the SwitchVersion UI has been destroyed
    | SelectedEndpoint(string, Memento.Endpoints.entry, bool)
    | SelectedDownloadAction(bool, string) // downloaded, versionString
    | SelectedOpenFolder(string)
    | SelectedNoInstallations
    | UpdatedEndpoints(array<(string, Memento.Endpoints.endpoint, option<string>, bool)>) // array of (path, endpoint, optional error, isSelected)
    | Others(string)

  let toString = event =>
    switch event {
    | Destroyed => "Destroyed"
    | SelectedEndpoint(path, entry, isSelected) =>
      "Endpoint: " ++
      path ++
      ", " ++
      Memento.Endpoints.endpointToString(entry.endpoint) ++ if isSelected {
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
    | UpdatedEndpoints(entries) =>
      "UpdatedEndpoints: " ++
      entries
      ->Array.map(((path, endpoint, error, isSelected)) =>
        path ++
        ": " ++
        switch endpoint {
        | Agda(Some(version)) => "Agda(" ++ version ++ ")"
        | Agda(None) => "Agda(None)"
        | ALS(Some(alsVersion, agdaVersion, _)) =>
          "ALS(" ++ alsVersion ++ ", " ++ agdaVersion ++ ")"
        | ALS(None) => "ALS(None)"
        | Unknown => "Unknown"
        } ++
        switch error {
        | Some(err) => " [Error: " ++ err ++ "]"
        | None => ""
        } ++ (isSelected ? " [Selected]" : "")
      )
      ->Array.join("\n")
    | Others(str) => str
    }
}

module Connection = {
  type t =
    | ConnectedToAgda(string, string) // path, version
    | ConnectedToALS(string, string, string) // path, ALS version, Agda version
    | Disconnected(string) // path

  let toString = event =>
    switch event {
    | ConnectedToAgda(path, version) => `ConnectedToAgda: ${path} - Agda v${version}`
    | ConnectedToALS(path, alsVersion, agdaVersion) =>
      `ConnectedToALS: ${path} - Agda v${agdaVersion} Language Server v${alsVersion}`
    | Disconnected(path) => `Disconnected: ${path}`
    }
}

type t =
  | CommandDispatched(Command.t)
  | CommandHandled(Command.t)
  | RequestSent(Request.t)
  | ResponseHandled(Response.t)
  | RegistryLookup(string, bool) // filepath, found
  | RegistryAdd(string) // filepath
  | RegistryRemove(string) // filepath
  | ParserFilepath(string, string) // input, output
  | TokensReset(string) // reason
  | AgdaModeOperation(string, string) // operation, filepath
  | SwitchVersionUI(SwitchVersion.t) // SwitchVersion UI event
  | Connection(Connection.t) // Connection event
  | Others(string) // generic string

let toString = log =>
  switch log {
  | CommandDispatched(command) => " <=== " ++ Command.toString(command)
  | RequestSent(request) => "   <- " ++ Request.toString(request)
  | ResponseHandled(response) => "    > " ++ Response.toString(response)
  | CommandHandled(command) => " ===> " ++ Command.toString(command)
  | RegistryLookup(filepath, found) =>
    "Registry lookup: " ++ filepath ++ " " ++ (found ? "found" : "not found")
  | RegistryAdd(filepath) => "Registry add: " ++ filepath
  | RegistryRemove(filepath) => "Registry remove: " ++ filepath
  | ParserFilepath(input, output) => "Parser.filepath: " ++ input ++ " -> " ++ output
  | TokensReset(reason) => "Tokens reset: " ++ reason
  | AgdaModeOperation(operation, filepath) => "AgdaMode." ++ operation ++ ": " ++ filepath
  | SwitchVersionUI(event) => "SwitchVersionUI: " ++ SwitchVersion.toString(event)
  | Connection(event) => "Connection: " ++ Connection.toString(event)
  | Others(str) => str
  }
