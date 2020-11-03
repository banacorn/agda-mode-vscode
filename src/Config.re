open VSCode;
module VSRange = Range;
open Belt;

// Agda path
let setAgdaPath = path =>
  Workspace.getConfiguration(Some("agdaMode"), None)
  ->WorkspaceConfiguration.updateGlobalSettings("agdaPath", path, None);
let getAgdaPath = () =>
  Workspace.getConfiguration(Some("agdaMode"), None)
  ->WorkspaceConfiguration.get("agdaPath");

// Library path
let getLibraryPath = () => {
  let raw =
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("libraryPath")
    ->Option.getWithDefault("");
  // split by comma, and clean them up
  Js.String.split(",", raw)
  ->Array.keep(x => x !== "")
  ->Array.map(Parser.filepath);
};
// Highlighting method
let getHighlightingMethod = () => {
  let raw =
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("highlighting.IPC");
  switch (raw) {
  | Some("Standard input/output") => true
  | _ => false
  };
};
// Semantic Highlighting
let getSemanticHighlighting = () => {
  let raw =
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("highlighting.semanticToken");
  switch (raw) {
  | Some(true) => true
  | _ => false
  };
};

// Backend
let getBackend = () => {
  let raw =
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("backend");
  switch (raw) {
  | Some("GHC") => "GHCNoMain"
  | Some("LaTeX") => "LaTeX"
  | Some("QuickLaTeX") => "QuickLaTeX"
  | _ => "GHCNoMain"
  };
};
