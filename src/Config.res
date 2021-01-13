open VSCode
module VSRange = Range
open Belt

// Agda version
let setAgdaVersion = path =>
  Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.updateGlobalSettings(
    "agdaVersion",
    path,
    None,
  )
let getAgdaVersion = () =>
  Workspace.getConfiguration(Some("agdaMode"), None)
  ->WorkspaceConfiguration.get("agdaVersion")
  ->Option.mapWithDefault("agda", Js.String.trim)

// Agda path
let setAgdaPath = path =>
  Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.updateGlobalSettings(
    "agdaPath",
    path,
    None,
  )
let getAgdaPath = () =>
  Workspace.getConfiguration(Some("agdaMode"), None)
  ->WorkspaceConfiguration.get("agdaPath")
  ->Option.mapWithDefault("", Js.String.trim)

// Panel mounting position
type mountAt = Bottom | Right
let setPanelMountingPosition = mountAt =>
  Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.updateGlobalSettings(
    "panelMountPosition",
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
      "panelMountPosition",
    )
  switch result {
  // | Some("left") => Left
  | Some("right") => Right
  | _ => Bottom
  }
}

// Library path
let getLibraryPath = () => {
  let raw =
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("libraryPath")
    ->Option.getWithDefault("")
  // split by comma, and clean them up
  Js.String.split(",", raw)->Array.keep(x => x !== "")->Array.map(Parser.filepath)
}
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
// Semantic Highlighting
let getSemanticHighlighting = () => {
  let raw =
    Workspace.getConfiguration(Some("agdaMode"), None)->WorkspaceConfiguration.get(
      "highlighting.semanticToken",
    )
  switch raw {
  | Some(true) => true
  | _ => false
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
