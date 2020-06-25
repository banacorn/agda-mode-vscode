open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Goal = Goal.Impl(Editor);

  type t =
    | Load
    | SolveConstraints(Command.Normalization.t, Goal.t)
    | SolveConstraintsGlobal(Command.Normalization.t)
    | Give(Goal.t)
    | Refine(Goal.t)
    | ElaborateAndGive(Command.Normalization.t, string, Goal.t)
    | Auto(Goal.t)
    | Case(Goal.t)
    | HelperFunctionType(Command.Normalization.t, string, Goal.t)
    | InferType(Command.Normalization.t, string, Goal.t)
    | InferTypeGlobal(Command.Normalization.t, string)
    | Context(Command.Normalization.t, Goal.t)
    | GoalType(Command.Normalization.t, Goal.t)
    | GoalTypeAndContext(Command.Normalization.t, Goal.t)
    | GoalTypeContextAndInferredType(Command.Normalization.t, string, Goal.t)
    | GoalTypeContextAndCheckedType(Command.Normalization.t, string, Goal.t)
    | ModuleContents(Command.Normalization.t, string, Goal.t)
    | ModuleContentsGlobal(Command.Normalization.t, string)
    | ComputeNormalForm(Command.ComputeMode.t, string, Goal.t)
    | ComputeNormalFormGlobal(Command.ComputeMode.t, string)
    | WhyInScope(string, Goal.t)
    | WhyInScopeGlobal(string);

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
      Js.Array.joinWith(
        ", ",
        Array.map(libraryPath, x => "\"" ++ Parser.filepath(x) ++ "\""),
      );
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

    | SolveConstraints(normalization, goal) =>
      let normalization = Command.Normalization.encode(normalization);
      let index = string_of_int(goal.index);

      commonPart(NonInteractive)
      ++ {j|( Cmd_solveOne $(normalization) $(index) noRange "" )|j};

    | SolveConstraintsGlobal(normalization) =>
      let normalization = Command.Normalization.encode(normalization);
      commonPart(NonInteractive) ++ {j|( Cmd_solveAll $(normalization) )|j};

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

    | Refine(goal) =>
      let index: string = string_of_int(goal.index);
      let content: string = Goal.getContent(goal, editor);
      let range: string = buildRange(goal);
      commonPart(NonInteractive)
      ++ {j|( Cmd_refine_or_intro False $(index) $(range) "$(content)" )|j};

    | ElaborateAndGive(normalization, expr, goal) =>
      let index = string_of_int(goal.index);
      let normalization = Command.Normalization.encode(normalization);
      let content = Parser.userInput(expr);
      commonPart(NonInteractive)
      ++ {j|( Cmd_elaborate_give $(normalization) $(index) noRange "$(content)" )|j};

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

    | Case(goal) =>
      let index: string = string_of_int(goal.index);
      let content: string = Goal.getContent(goal, editor);
      let range: string = buildRange(goal);
      commonPart(NonInteractive)
      ++ {j|( Cmd_make_case $(index) $(range) "$(content)" )|j};

    | HelperFunctionType(normalization, expr, goal) =>
      let index = string_of_int(goal.index);
      let normalization = Command.Normalization.encode(normalization);
      let content = Parser.userInput(expr);
      commonPart(NonInteractive)
      ++ {j|( Cmd_helper_function $(normalization) $(index) noRange "$(content)" )|j};

    | InferType(normalization, expr, goal) =>
      let index = string_of_int(goal.index);
      let normalization = Command.Normalization.encode(normalization);
      let content = Parser.userInput(expr);
      commonPart(NonInteractive)
      ++ {j|( Cmd_infer $(normalization) $(index) noRange "$(content)" )|j};

    | InferTypeGlobal(normalization, expr) =>
      let normalization = Command.Normalization.encode(normalization);
      let content = Parser.userInput(expr);

      commonPart(None)
      ++ {j|( Cmd_infer_toplevel $(normalization) "$(content)" )|j};

    | Context(normalization, goal) =>
      let index = string_of_int(goal.index);
      let normalization = Command.Normalization.encode(normalization);
      commonPart(NonInteractive)
      ++ {j|( Cmd_context $(normalization) $(index) noRange "" )|j};

    | GoalType(normalization, goal) =>
      let index = string_of_int(goal.index);
      let normalization = Command.Normalization.encode(normalization);
      commonPart(NonInteractive)
      ++ {j|( Cmd_goal_type $(normalization) $(index) noRange "" )|j};

    | GoalTypeAndContext(normalization, goal) =>
      let index: string = string_of_int(goal.index);
      let normalization: string = Command.Normalization.encode(normalization);
      commonPart(NonInteractive)
      ++ {j|( Cmd_goal_type_context $(normalization) $(index) noRange "" )|j};

    | GoalTypeContextAndInferredType(normalization, expr, goal) =>
      let index: string = string_of_int(goal.index);
      let normalization: string = Command.Normalization.encode(normalization);
      let content = Parser.userInput(expr);
      commonPart(NonInteractive)
      ++ {j|( Cmd_goal_type_context_infer $(normalization) $(index) noRange "$(content)" )|j};

    | GoalTypeContextAndCheckedType(normalization, expr, goal) =>
      let index: string = string_of_int(goal.index);
      let normalization: string = Command.Normalization.encode(normalization);
      let content = Parser.userInput(expr);
      commonPart(NonInteractive)
      ++ {j|( Cmd_goal_type_context_check $(normalization) $(index) noRange "$(content)" )|j};

    | ModuleContents(normalization, expr, goal) =>
      let index: string = string_of_int(goal.index);
      let normalization: string = Command.Normalization.encode(normalization);
      let content = Parser.userInput(expr);

      commonPart(NonInteractive)
      ++ {j|( Cmd_show_module_contents $(normalization) $(index) noRange "$(content)" )|j};

    | ModuleContentsGlobal(normalization, expr) =>
      let normalization: string = Command.Normalization.encode(normalization);
      let content = Parser.userInput(expr);

      commonPart(None)
      ++ {j|( Cmd_show_module_contents_toplevel $(normalization) "$(content)" )|j};

    | ComputeNormalForm(computeMode, expr, goal) =>
      let index: string = string_of_int(goal.index);
      let ignoreAbstract: string =
        string_of_bool(Command.ComputeMode.ignoreAbstract(computeMode));
      let computeMode: string = Command.ComputeMode.encode(computeMode);
      let content: string = Parser.userInput(expr);

      if (Util.Version.gte(version, "2.5.2")) {
        commonPart(NonInteractive)
        ++ {j|( Cmd_compute $(computeMode) $(index) noRange "$(content)" )|j};
      } else {
        commonPart(NonInteractive)
        ++ {j|( Cmd_compute $(ignoreAbstract) $(index) noRange "$(content)" )|j};
      };

    | ComputeNormalFormGlobal(computeMode, expr) =>
      let ignoreAbstract: string =
        string_of_bool(Command.ComputeMode.ignoreAbstract(computeMode));
      let computeMode: string = Command.ComputeMode.encode(computeMode);
      let content = Parser.userInput(expr);

      if (Util.Version.gte(version, "2.5.2")) {
        commonPart(NonInteractive)
        ++ {j|( Cmd_compute_toplevel $(computeMode) "$(content)" )|j};
      } else {
        commonPart(NonInteractive)
        ++ {j|( Cmd_compute_toplevel $(ignoreAbstract) "$(content)" )|j};
      };

    | WhyInScope(expr, goal) =>
      let index: string = string_of_int(goal.index);
      let content: string = Parser.userInput(expr);

      commonPart(NonInteractive)
      ++ {j|( Cmd_why_in_scope $(index) noRange "$(content)" )|j};

    | WhyInScopeGlobal(expr) =>
      let content = Parser.userInput(expr);
      commonPart(None) ++ {j|( Cmd_why_in_scope_toplevel "$(content)" )|j};
    };
  };
};