// datatype for logging
module SwitchVersion = {
  type t =
    | Destroyed // when the SwitchVersion UI has been destroyed
    | SelectedEndpoint(string, Memento.Endpoints.entry, bool)
    | SelectedDownloadAction(bool, string) // downloaded, versionString
    | SelectedOpenFolder(string)
    | SelectedNoInstallations
    | UpdatedEndpoints(array<(string, Memento.Endpoints.endpoint, option<string>, bool)>) // array of (path, endpoint, optional error, isSelected)
    | SelectionCompleted // when onSelection handler has completed all async operations
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
    | SelectionCompleted => "Selection Completed"
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
    | ConnectedToALS(string, option<(string, string)>) // path, ALS version, Agda version
    | Disconnected(string) // path

  let toString = event =>
    switch event {
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
  | Others(str) => str
  }

let isConfig = log =>
  switch log {
  | Config(_) => true
  | _ => false
  }

let isConnection = log =>
  switch log {
  | Connection(_) => true
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
