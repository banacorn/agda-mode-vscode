open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Goal = Goal.Impl(Editor);

  type t =
    | Load
    | Give(Goal.t)
    | Auto(Goal.t)
    | InferType(Command.Normalization.t, string, Goal.t)
    | InferTypeGlobal(Command.Normalization.t, string)
    | GoalType(Command.Normalization.t, Goal.t);

  // How much highlighting should be sent to the user interface?
  type highlightingLevel =
    | None
    | NonInteractive
    | Interactive;

  // encode Request.t to String
  let encode =
      (
        editor: Editor.editor,
        version,
        filepath: string,
        libraryPath: array(string),
        highlightingMethod: bool,
        request,
      ) => {
    let libraryPath: string = {
      // add the current directory to the front
      Js.Array.unshift(".", libraryPath)->ignore;
      // add quotes and then concatenate the paths with commas
      libraryPath
      ->Array.map(x => "\"" ++ Parser.filepath(x) ++ "\"")
      ->Js.String.concatMany(", ");
    };

    let highlightingMethod = highlightingMethod ? "Direct" : "Indirect";

    // the common first half
    let commonPart: highlightingLevel => string =
      level => {
        let level =
          switch (level) {
          | None => "None"
          | NonInteractive => "NonInteractive"
          | Interactive => "Interactive"
          };
        "IOTCM \"" ++ filepath ++ "\" " ++ level ++ " " ++ highlightingMethod;
      };

    let buildRange = goal =>
      if (Util.Version.gte(version, "2.5.1")) {
        Goal.buildHaskellRange(editor, goal, false, filepath);
      } else {
        Goal.buildHaskellRange(editor, goal, true, filepath);
      };

    // assemble them
    switch (request) {
    | Load =>
      if (Util.Version.gte(version, "2.5.0")) {
        commonPart(NonInteractive) ++ {j|( Cmd_load "$(filepath)" [] )|j};
      } else {
        commonPart(NonInteractive)
        ++ {j|( Cmd_load "$(filepath)" [$(libraryPath)] )|j};
      }

    // Related issue and commit of agda/agda
    // https://github.com/agda/agda/issues/2730
    // https://github.com/agda/agda/commit/021e6d24f47bac462d8bc88e2ea685d6156197c4
    | Give(goal) =>
      let index: string = string_of_int(goal.index);
      let content: string = Goal.getContent(goal, editor);
      let range: string = buildRange(goal);
      if (Util.Version.gte(version, "2.5.3")) {
        commonPart(NonInteractive)
        ++ {j|( Cmd_give WithoutForce $(index) $(range) "$(content)" )|j};
      } else {
        commonPart(NonInteractive)
        ++ {j|( Cmd_give $(index) $(range) "$(content)" )|j};
      };

    | Auto(goal) =>
      let index: string = string_of_int(goal.index);
      let content: string = Goal.getContent(goal, editor);
      let range: string = buildRange(goal);
      if (Util.Version.gte(version, "2.6.0.1")) {
        // after 2.6.0.1
        commonPart(NonInteractive)
        ++ {j|( Cmd_autoOne $(index) $(range) "$(content)" )|j};
      } else {
        // the old way
        commonPart(NonInteractive)
        ++ {j|( Cmd_auto $(index) $(range) "$(content)" )|j};
      };

    | InferType(normalization, expr, goal) =>
      let index = string_of_int(goal.index);
      let normalization = Command.Normalization.toString(normalization);
      let content = Parser.userInput(expr);
      commonPart(NonInteractive)
      ++ {j|( Cmd_infer $(normalization) $(index) noRange "$(content)" )|j};

    | InferTypeGlobal(normalization, expr) =>
      let normalization = Command.Normalization.toString(normalization);
      let content = Parser.userInput(expr);

      commonPart(None)
      ++ {j|( Cmd_infer_toplevel $(normalization) "$(content)" )|j};

    | GoalType(normalization, goal) =>
      let index = string_of_int(goal.index);
      let normalization = Command.Normalization.toString(normalization);
      commonPart(NonInteractive)
      ++ {j|( Cmd_goal_type $(normalization) $(index) noRange "" )|j};
    };
  };
};