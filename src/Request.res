open Belt

type t =
  | Load
  | Compile
  | ToggleDisplayOfImplicitArguments
  | ShowConstraints
  | SolveConstraints(Command.Normalization.t, Goal.t)
  | SolveConstraintsGlobal(Command.Normalization.t)
  | ShowGoals
  | SearchAbout(Command.Normalization.t, string)
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
  | WhyInScopeGlobal(string)

let toString = x =>
  switch x {
  | Load => "Load"
  | Compile => "Compile"
  | ToggleDisplayOfImplicitArguments => "ToggleDisplayOfImplicitArguments"
  | ShowConstraints => "ShowConstraints"
  | SolveConstraints(_, _) => "SolveConstraints"
  | SolveConstraintsGlobal(_) => "SolveConstraintsGlobal"
  | ShowGoals => "ShowGoals"
  | SearchAbout(_, _) => "SearchAbout"
  | Give(_) => "Give"
  | Refine(_) => "Refine"
  | ElaborateAndGive(_, _, _) => "ElaborateAndGive"
  | Auto(_) => "Auto"
  | Case(_) => "Case"
  | HelperFunctionType(_, _, _) => "HelperFunctionType"
  | InferType(_, _, _) => "InferType"
  | InferTypeGlobal(_, _) => "InferTypeGlobal"
  | Context(_, _) => "Context"
  | GoalType(_, _) => "GoalType"
  | GoalTypeAndContext(_, _) => "GoalTypeAndContext"
  | GoalTypeContextAndInferredType(_, _, _) => "GoalTypeContextAndInferredType"
  | GoalTypeContextAndCheckedType(_, _, _) => "GoalTypeContextAndCheckedType"
  | ModuleContents(_, _, _) => "ModuleContents"
  | ModuleContentsGlobal(_, _) => "ModuleContentsGlobal"
  | ComputeNormalForm(_, _, _) => "ComputeNormalForm"
  | ComputeNormalFormGlobal(_, _) => "ComputeNormalFormGlobal"
  | WhyInScope(_, _) => "WhyInScope"
  | WhyInScopeGlobal(_) => "WhyInScopeGlobal"
  }

// How much highlighting should be sent to the user interface?
type highlightingLevel =
  | None
  | NonInteractive
  | Interactive

// encode Request.t to String
let encode = (
  document: VSCode.TextDocument.t,
  version,
  filepath: string,
  backend: string,
  libraryPath: array<string>,
  highlightingMethod: bool,
  request,
) => {
  let libraryPath: string = {
    // add the current directory to the front
    Js.Array.unshift(".", libraryPath)->ignore
    // add quotes and then concatenate the paths with commas
    Js.Array.joinWith(", ", Array.map(libraryPath, x => "\"" ++ (Parser.filepath(x) ++ "\"")))
  }

  let highlightingMethod = highlightingMethod ? "Direct" : "Indirect"

  // the common first half
  let commonPart: highlightingLevel => string = level => {
    let level = switch level {
    | None => "None"
    | NonInteractive => "NonInteractive"
    | Interactive => "Interactive"
    }
    "IOTCM \"" ++ (filepath ++ ("\" " ++ (level ++ (" " ++ highlightingMethod))))
  }

  let buildRange = goal => Goal.buildHaskellRange(goal, document, version, filepath)

  // assemble them
  switch request {
  | Load =>
    if Util.Version.gte(version, "2.5.0") {
      j`${commonPart(NonInteractive)}( Cmd_load "$(filepath)" [] )`
    } else {
      j`${commonPart(NonInteractive)}( Cmd_load "$(filepath)" [$(libraryPath)] )`
    }

  | Compile =>
    if Util.Version.gte(version, "2.5.0") {
      j`${commonPart(NonInteractive)}( Cmd_compile $(backend) "$(filepath)" [] )`
    } else {
      j`${commonPart(NonInteractive)}( Cmd_compile $(backend) "$(filepath)" [$(libraryPath)] )`
    }

  | ToggleDisplayOfImplicitArguments => j`${commonPart(NonInteractive)}( ToggleImplicitArgs )`

  | ShowConstraints => j`${commonPart(NonInteractive)}( Cmd_constraints )`

  | SolveConstraints(normalization, goal) =>
    let normalization = Command.Normalization.encode(normalization)
    let index = string_of_int(goal.index)

    j`${commonPart(NonInteractive)}( Cmd_solveOne $(normalization) $(index) noRange "" )`

  | SolveConstraintsGlobal(normalization) =>
    let normalization = Command.Normalization.encode(normalization)
    j`${commonPart(NonInteractive)}( Cmd_solveAll $(normalization) )`

  | ShowGoals => j`${commonPart(NonInteractive)}( Cmd_metas )`

  | SearchAbout(normalization, expr) =>
    let normalization = Command.Normalization.encode(normalization)
    let content = Parser.userInput(expr)
    j`${commonPart(NonInteractive)}( Cmd_search_about_toplevel $(normalization) "$(content)" )`

  // Related issue and commit of agda/agda
  // https://github.com/agda/agda/issues/2730
  // https://github.com/agda/agda/commit/021e6d24f47bac462d8bc88e2ea685d6156197c4
  | Give(goal) =>
    let index: string = string_of_int(goal.index)
    let content: string = Goal.getContent(goal, document)
    let range: string = buildRange(goal)
    if Util.Version.gte(version, "2.5.3") {
      j`${commonPart(NonInteractive)}( Cmd_give WithoutForce $(index) $(range) "$(content)" )`
    } else {
      j`${commonPart(NonInteractive)}( Cmd_give $(index) $(range) "$(content)" )`
    }

  | Refine(goal) =>
    let index: string = string_of_int(goal.index)
    let content: string = Goal.getContent(goal, document)
    let range: string = buildRange(goal)
    j`${commonPart(NonInteractive)}( Cmd_refine_or_intro False $(index) $(range) "$(content)" )`

  | ElaborateAndGive(normalization, expr, goal) =>
    let index = string_of_int(goal.index)
    let normalization = Command.Normalization.encode(normalization)
    let content = Parser.userInput(expr)

    j`${commonPart(
        NonInteractive,
      )}( Cmd_elaborate_give $(normalization) $(index) noRange "$(content)" )`

  | Auto(goal) =>
    let index: string = string_of_int(goal.index)
    let content: string = Goal.getContent(goal, document)
    let range: string = buildRange(goal)
    if Util.Version.gte(version, "2.6.0.1") {
      // after 2.6.0.1
      j`${commonPart(NonInteractive)}( Cmd_autoOne $(index) $(range) "$(content)" )`
    } else {
      // the old way
      j`${commonPart(NonInteractive)}( Cmd_auto $(index) $(range) "$(content)" )`
    }

  | Case(goal) =>
    let index: string = string_of_int(goal.index)
    let content: string = Goal.getContent(goal, document)
    let range: string = buildRange(goal)
    j`${commonPart(NonInteractive)}( Cmd_make_case $(index) $(range) "$(content)" )`

  | HelperFunctionType(normalization, expr, goal) =>
    let index = string_of_int(goal.index)
    let normalization = Command.Normalization.encode(normalization)
    let content = Parser.userInput(expr)

    j`${commonPart(
        NonInteractive,
      )}( Cmd_helper_function $(normalization) $(index) noRange "$(content)" )`

  | InferType(normalization, expr, goal) =>
    let index = string_of_int(goal.index)
    let normalization = Command.Normalization.encode(normalization)
    let content = Parser.userInput(expr)
    j`${commonPart(NonInteractive)}( Cmd_infer $(normalization) $(index) noRange "$(content)" )`

  | InferTypeGlobal(normalization, expr) =>
    let normalization = Command.Normalization.encode(normalization)
    let content = Parser.userInput(expr)

    j`${commonPart(None)}( Cmd_infer_toplevel $(normalization) "$(content)" )`

  | Context(normalization, goal) =>
    let index = string_of_int(goal.index)
    let normalization = Command.Normalization.encode(normalization)
    j`${commonPart(NonInteractive)}( Cmd_context $(normalization) $(index) noRange "" )`

  | GoalType(normalization, goal) =>
    let index = string_of_int(goal.index)
    let normalization = Command.Normalization.encode(normalization)
    j`${commonPart(NonInteractive)}( Cmd_goal_type $(normalization) $(index) noRange "" )`

  | GoalTypeAndContext(normalization, goal) =>
    let index: string = string_of_int(goal.index)
    let normalization: string = Command.Normalization.encode(normalization)
    j`${commonPart(NonInteractive)}( Cmd_goal_type_context $(normalization) $(index) noRange "" )`

  | GoalTypeContextAndInferredType(normalization, expr, goal) =>
    let index: string = string_of_int(goal.index)
    let normalization: string = Command.Normalization.encode(normalization)
    let content = Parser.userInput(expr)

    j`${commonPart(
        NonInteractive,
      )}( Cmd_goal_type_context_infer $(normalization) $(index) noRange "$(content)" )`

  | GoalTypeContextAndCheckedType(normalization, expr, goal) =>
    let index: string = string_of_int(goal.index)
    let normalization: string = Command.Normalization.encode(normalization)
    let content = Parser.userInput(expr)

    j`${commonPart(
        NonInteractive,
      )}( Cmd_goal_type_context_check $(normalization) $(index) noRange "$(content)" )`

  | ModuleContents(normalization, expr, goal) =>
    let index: string = string_of_int(goal.index)
    let normalization: string = Command.Normalization.encode(normalization)
    let content = Parser.userInput(expr)

    j`${commonPart(
        NonInteractive,
      )}( Cmd_show_module_contents $(normalization) $(index) noRange "$(content)" )`

  | ModuleContentsGlobal(normalization, expr) =>
    let normalization: string = Command.Normalization.encode(normalization)
    let content = Parser.userInput(expr)

    j`${commonPart(None)}( Cmd_show_module_contents_toplevel $(normalization) "$(content)" )`

  | ComputeNormalForm(computeMode, expr, goal) =>
    let index: string = string_of_int(goal.index)
    let ignoreAbstract: string = string_of_bool(Command.ComputeMode.ignoreAbstract(computeMode))
    let computeMode: string = Command.ComputeMode.encode(computeMode)
    let content: string = Parser.userInput(expr)

    if Util.Version.gte(version, "2.5.2") {
      j`${commonPart(NonInteractive)}( Cmd_compute $(computeMode) $(index) noRange "$(content)" )`
    } else {
      j`${commonPart(
          NonInteractive,
        )}( Cmd_compute $(ignoreAbstract) $(index) noRange "$(content)" )`
    }

  | ComputeNormalFormGlobal(computeMode, expr) =>
    let ignoreAbstract: string = string_of_bool(Command.ComputeMode.ignoreAbstract(computeMode))
    let computeMode: string = Command.ComputeMode.encode(computeMode)
    let content = Parser.userInput(expr)

    if Util.Version.gte(version, "2.5.2") {
      j`${commonPart(NonInteractive)}( Cmd_compute_toplevel $(computeMode) "$(content)" )`
    } else {
      j`${commonPart(NonInteractive)}( Cmd_compute_toplevel $(ignoreAbstract) "$(content)" )`
    }

  | WhyInScope(expr, goal) =>
    let index: string = string_of_int(goal.index)
    let content: string = Parser.userInput(expr)

    j`${commonPart(NonInteractive)}( Cmd_why_in_scope $(index) noRange "$(content)" )`

  | WhyInScopeGlobal(expr) =>
    let content = Parser.userInput(expr)
    j`${commonPart(None)}( Cmd_why_in_scope_toplevel "$(content)" )`
  }
}
