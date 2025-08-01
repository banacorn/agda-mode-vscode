open VSCode
module VSRange = Range

// this flag should be set as TRUE when testing
let inTestingMode = ref(false)

// for enabling experimental features like WASM support
module DevMode = {
  // Default value for dev mode
  let defaultValue = false

  // Parse and validate dev mode value from VSCode config
  let parseFromConfig = (configValue: option<JSON.t>): bool =>
    switch configValue {
    | Some(value) =>
      switch value {
      | JSON.Boolean(true) => true
      | JSON.Boolean(false) => false
      | _ => defaultValue
      }
    | None => defaultValue
    }

  let get = () =>
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("devMode.enabled")
    ->parseFromConfig

  let set = (value: bool) => {
    Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.updateGlobalSettings(
      "devMode.enabled",
      value,
      None,
    )
  }
}

module Connection = {
  // in testing mode, configs are read and written from here instead
  let agdaVersionInTestingMode = ref("agda")
  let agdaPathsInTestingMode = ref([])
  let agdaPathsInTestingMode2 = ref([])
  let useAgdaLanguageServerInTestingMode = ref(false)

  // Agda version
  let setAgdaVersion = path =>
    if inTestingMode.contents {
      agdaVersionInTestingMode := path
      Promise.resolve()
    } else {
      Workspace.getConfiguration(
        Some("agdaMode"),
        None,
      )->WorkspaceConfiguration.updateGlobalSettings("connection.agdaVersion", path, None)
    }

  let getAgdaVersion = () =>
    if inTestingMode.contents {
      agdaVersionInTestingMode.contents
    } else {
      Workspace.getConfiguration(Some("agdaMode"), None)
      ->WorkspaceConfiguration.get("connection.agdaVersion")
      ->Option.map(String.trim)
      ->Option.flatMap(s => s == "" ? None : Some(s))
      ->Option.getOr("agda")
    }

  // overwrite all Agda paths
  let setAgdaPaths = paths =>
    if inTestingMode.contents {
      agdaPathsInTestingMode := paths
      Promise.resolve()
    } else {
      // use the original
      let paths: array<string> = paths->Array.map(Connection__URI.getOriginalPath)
      Workspace.getConfiguration(
        Some("agdaMode"),
        None,
      )->WorkspaceConfiguration.updateGlobalSettings("connection.paths", paths, None)
    }

  // expects an array of JSON strings
  let parseAgdaPaths = (raw: JSON.t) => {
    let rawPaths: array<string> = switch raw {
    | Array(strings) =>
      strings->Array.filterMap(s =>
        switch s {
        | String(s) => Some(s)
        | _ => None
        }
      )
    | _ => []
    }
    rawPaths->Array.toReversed
  }

  let getAgdaPaths = () =>
    if inTestingMode.contents {
      agdaPathsInTestingMode.contents
    } else {
      let paths =
        Workspace.getConfiguration(Some("agdaMode"), None)
        ->WorkspaceConfiguration.get("connection.paths")
        ->Option.getOr(JSON.Null)
        ->parseAgdaPaths

      paths->Array.map(Connection__URI.parse)
    }

  let getAgdaPaths2 = () =>
    if inTestingMode.contents {
      agdaPathsInTestingMode2.contents
    } else {
      Workspace.getConfiguration(Some("agdaMode"), None)
      ->WorkspaceConfiguration.get("connection.paths")
      ->Option.getOr(JSON.Null)
      ->parseAgdaPaths
    }

  // new path is APPENDED to the end of the list
  // no-op if it's already in the list
  let addAgdaPath = path => {
    let paths = getAgdaPaths()
    let alreadyExists =
      paths->Array.reduce(false, (acc, p) => acc || Connection__URI.equal(p, path))

    if alreadyExists {
      Promise.resolve()
    } else {
      let newPaths = Array.concat(paths, [path])
      if inTestingMode.contents {
        agdaPathsInTestingMode := newPaths
        Promise.resolve()
      } else {
        let newPaths: array<string> = paths->Array.map(Connection__URI.getOriginalPath)
        Workspace.getConfiguration(
          Some("agdaMode"),
          None,
        )->WorkspaceConfiguration.updateGlobalSettings("connection.paths", newPaths, None)
      }
    }
  }

  // Agda command-line options
  let getCommandLineOptions = () =>
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("connection.commandLineOptions")
    ->Option.mapOr([], s => String.trim(s)->String.split(" "))
    ->Array.filter(s => String.trim(s) != "")

  // Agda Language Server port
  let getAgdaLanguageServerPort = () => {
    let raw =
      Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
        "connection.agdaLanguageServerPort",
      )
    switch raw {
    | Some(port) => port
    | _ => 4096
    }
  }

  // Agda Language Server command-line options
  let getAgdaLanguageServerCommandLineOptions = () =>
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("connection.agdaLanguageServerOptions")
    ->Option.mapOr([], s => String.trim(s)->String.split(" "))
    ->Array.filter(s => String.trim(s) != "")

  // Download policy when Agda or Agda Language Server is missing
  module DownloadPolicy = {
    type t = Yes | No | Undecided

    // in testing mode, configs are read and written from here instead
    let testingMode = ref(Undecided)

    let toString = policy =>
      switch policy {
      | Yes => "Yes"
      | No => "No, and don't ask again"
      | Undecided => "Undecided"
      }

    let fromString = s =>
      switch s {
      | "Yes" => Yes
      | "No, and don't ask again" => No
      | _ => Undecided
      }

    let get = () => {
      if inTestingMode.contents {
        testingMode.contents
      } else {
        Workspace.getConfiguration(Some("agdaMode"), None)
        ->WorkspaceConfiguration.get("connection.downloadPolicy")
        ->Option.mapOr(Undecided, fromString)
      }
    }

    let set = policy =>
      if inTestingMode.contents {
        testingMode := policy
        Promise.resolve()
      } else {
        Workspace.getConfiguration(
          Some("agdaMode"),
          None,
        )->WorkspaceConfiguration.updateGlobalSettings(
          "connection.downloadPolicy",
          toString(policy),
          None,
        )
      }
  }
}

module View = {
  // Panel mounting position
  type mountAt = Bottom | Right
  let setPanelMountingPosition = mountAt =>
    Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.updateGlobalSettings(
      "view.panelMountPosition",
      switch mountAt {
      | Bottom => "bottom"
      // | Left => "left"
      | Right => "right"
      },
      None,
    )
  let getPanelMountingPosition = () => {
    let result =
      Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
        "view.panelMountPosition",
      )
    switch result {
    // | Some("left") => Left
    | Some("right") => Right
    | _ => Bottom
    }
  }
}
// Library path
let getLibraryPath = () => {
  let raw =
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("libraryPath")
    ->Option.getOr("")
  // split by comma, and clean them up
  raw->String.split(",")->Array.filter(x => x !== "")->Array.map(Parser.filepath)
}

module Highlighting = {
  // Highlighting method
  let getHighlightingMethod = () => {
    let raw =
      Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
        "highlighting.IPC",
      )
    switch raw {
    | Some("Temporary Files") => false
    | _ => true
    }
  }
}

// Backend
let getBackend = () => {
  let raw =
    Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get("backend")
  switch raw {
  | Some("GHC") => "GHCNoMain"
  | Some("LaTeX") => "LaTeX"
  | Some("QuickLaTeX") => "QuickLaTeX"
  | _ => "GHCNoMain"
  }
}

module InputMethod = {
  let getEnabled = () => {
    let raw =
      Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
        "inputMethod.enabled",
      )
    switch raw {
    | Some(true) => true
    | Some(false) => false
    | _ => true // enabled by default
    }
  }
}

module Buffer = {
  let getFontSize = () => {
    let config = Workspace.getConfiguration(Some("agdaMode"), None)
    let editorFontSize = switch Workspace.getConfiguration(
      Some("editor"),
      None,
    )->WorkspaceConfiguration.get("fontSize") {
    | Some(n) => n
    | _ => 14
    }
    let size = switch config->WorkspaceConfiguration.get("buffer.fontSize") {
    | Some(m) =>
      if m == null {
        editorFontSize
      } else {
        m
      }
    | None => editorFontSize
    }
    size->Int.toString
  }
}
