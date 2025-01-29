open VSCode
module VSRange = Range

// this flag should be set as TRUE when testing
let inTestingMode = ref(false)

module Connection = {
  // in testing mode, configs are read and written from here instead
  let agdaVersionInTestingMode = ref("agda")
  let agdaPathsInTestingMode = ref([])
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

  // paths returned here are REVERSED because it's easier for users to append stuff in the configuration
  let getAgdaPaths = () =>
    if inTestingMode.contents {
      agdaPathsInTestingMode.contents
    } else {
      let rawPaths: option<array<string>> =
        Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
          "connection.paths",
        )

      let paths =
        rawPaths
        ->Option.getOr([])
        ->Array.filter(s => Parser.filepath(s) != "")
      Array.reverse(paths)
      paths
    }

  // new path is APPENDED to the end of the list
  // no-op if it's already in the list
  let addAgdaPath = path => {
    let path = path->Parser.filepath

    let paths = getAgdaPaths()
    let alreadyExists = paths->Array.reduce(false, (acc, p) => acc || p == path)

    if alreadyExists {
      Promise.resolve()
    } else {
      let newPaths = Array.concat(paths, [path])
      if inTestingMode.contents {
        agdaPathsInTestingMode := newPaths
        Promise.resolve()
      } else {
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

  // Agda Language Server
  let getUseAgdaLanguageServer = () =>
    if inTestingMode.contents {
      useAgdaLanguageServerInTestingMode.contents
    } else {
      let raw =
        Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
          "connection.agdaLanguageServer",
        )
      switch raw {
      | Some(true) => true
      | _ => false
      }
    }
  let setUseAgdaLanguageServer = (mode: bool) =>
    if inTestingMode.contents {
      useAgdaLanguageServerInTestingMode := mode
      Promise.resolve()
    } else {
      Workspace.getConfiguration(
        Some("agdaMode"),
        None,
      )->WorkspaceConfiguration.updateGlobalSettings("connection.agdaLanguageServer", mode, None)
    }
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

  // Ask user if they want to download Agda or ALS if they are not found
  module Download = {
    type downloadPrompt = AlwaysAsk | NeverAsk

    let getDownloadPrompt = () => {
      Workspace.getConfiguration(Some("agdaMode"), None)
      ->WorkspaceConfiguration.get("connection.downloadPrompt")
      ->Option.mapOr(AlwaysAsk, s =>
        switch s {
        | "always" => AlwaysAsk
        | "never" => NeverAsk
        | _ => AlwaysAsk
        }
      )
    }

    let setDownloadPrompt = prompt =>
      Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.updateGlobalSettings(
        "connection.downloadPrompt",
        switch prompt {
        | AlwaysAsk => "always"
        | NeverAsk => "never"
        },
        None,
      )
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
    | Some("Standard input/output") => true
    | _ => false
    }
  }
  // Highlight stuff with theme colors (LSP Semantic Highlighting)
  let getHighlightWithThemeColors = () => {
    let raw =
      Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
        "highlighting.getHighlightWithThemeColors",
      )
    switch raw {
    | Some(true) => true
    | Some(false) => false
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
  let getEnable = () => {
    let raw =
      Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
        "inputMethod.enable",
      )
    switch raw {
    | Some(true) => true
    | Some(false) => false
    | _ => true // enabled by default
    }
  }
  let getActivationKey = () => {
    let raw =
      Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
        "inputMethod.activationKey",
      )
    switch raw {
    | Some(s) =>
      switch s->String.substring(~start=0, ~end=1) {
      | "" => "\\"
      | key => key
      }
    | _ => "\\"
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
