module Normalization = {
  type t =
    | Simplified
    | Instantiated
    | Normalised;

  // for Agda
  let encode =
    fun
    | Simplified => "Simplified"
    | Instantiated => "Instantiated"
    | Normalised => "Normalised";

  // for human
  let toString =
    fun
    | Simplified => "(simplified)"
    | Instantiated => "(instantiated)"
    | Normalised => "(normalised)";
};

module ComputeMode = {
  type t =
    | DefaultCompute
    | IgnoreAbstract
    | UseShowInstance;

  // for Agda
  let encode =
    fun
    | DefaultCompute => "DefaultCompute"
    | IgnoreAbstract => "IgnoreAbstract"
    | UseShowInstance => "UseShowInstance";

  let ignoreAbstract =
    fun
    | DefaultCompute => false
    | IgnoreAbstract => true
    | UseShowInstance => true;
};

module InputMethod = {
  type t =
    | Activate
    | Deactivate
    | Update(string, Translator.translation, int)
    | InsertChar(string)
    | ChooseSymbol(string)
    | MoveUp
    | MoveRight
    | MoveDown
    | MoveLeft;

  let toString =
    fun
    | Activate => "Activate"
    | Deactivate => "Deactivate"
    | Update(_, _, _) => "Update"
    | InsertChar(char) => "InsertChar '" ++ char ++ "'"
    | ChooseSymbol(symbol) => "ChooseSymbol '" ++ symbol ++ "'"
    | MoveUp => "MoveUp"
    | MoveRight => "MoveRight"
    | MoveDown => "MoveDown"
    | MoveLeft => "MoveLeft";
};

type t =
  | Load
  | Quit
  | Restart
  | Refresh
  | Compile
  | ToggleDisplayOfImplicitArguments
  | ShowConstraints
  | SolveConstraints(Normalization.t)
  | ShowGoals
  | NextGoal
  | PreviousGoal
  | SearchAbout(Normalization.t)
  | Give
  | Refine
  | ElaborateAndGive(Normalization.t)
  | Auto
  | Case
  | HelperFunctionType(Normalization.t)
  | InferType(Normalization.t)
  | Context(Normalization.t)
  | GoalType(Normalization.t)
  | GoalTypeAndContext(Normalization.t)
  | EventFromView(View.EventFromView.t)
  | GoalTypeContextAndInferredType(Normalization.t)
  | GoalTypeContextAndCheckedType(Normalization.t)
  | ModuleContents(Normalization.t)
  | ComputeNormalForm(ComputeMode.t)
  | WhyInScope
  | Escape
  | InputMethod(InputMethod.t);

// for registering Keybindings
let names: array((t, string)) = [|
  (Load, "load"),
  (Quit, "quit"),
  (Restart, "restart"),
  (Compile, "compile"),
  (ToggleDisplayOfImplicitArguments, "toggle-display-of-implicit-arguments"),
  (ShowConstraints, "show-constraints"),
  (SolveConstraints(Simplified), "solve-constraints[Simplified]"),
  (SolveConstraints(Instantiated), "solve-constraints[Instantiated]"),
  (SolveConstraints(Normalised), "solve-constraints[Normalised]"),
  (ShowGoals, "show-goals"),
  (NextGoal, "next-goal"),
  (PreviousGoal, "previous-goal"),
  (SearchAbout(Simplified), "search-about[Simplified]"),
  (SearchAbout(Instantiated), "search-about[Instantiated]"),
  (SearchAbout(Normalised), "search-about[Normalised]"),
  (Give, "give"),
  (Refine, "refine"),
  (ElaborateAndGive(Simplified), "elaborate-and-give[Simplified]"),
  (ElaborateAndGive(Instantiated), "elaborate-and-give[Instantiated]"),
  (ElaborateAndGive(Normalised), "elaborate-and-give[Normalised]"),
  (Auto, "auto"),
  (Case, "case"),
  (HelperFunctionType(Simplified), "helper-function-type[Simplified]"),
  (HelperFunctionType(Instantiated), "helper-function-type[Instantiated]"),
  (HelperFunctionType(Normalised), "helper-function-type[Normalised]"),
  (InferType(Simplified), "infer-type[Simplified]"),
  (InferType(Instantiated), "infer-type[Instantiated]"),
  (InferType(Normalised), "infer-type[Normalised]"),
  (Context(Simplified), "context[Simplified]"),
  (Context(Instantiated), "context[Instantiated]"),
  (Context(Normalised), "context[Normalised]"),
  (GoalType(Simplified), "goal-type[Simplified]"),
  (GoalType(Instantiated), "goal-type[Instantiated]"),
  (GoalType(Normalised), "goal-type[Normalised]"),
  (GoalTypeAndContext(Simplified), "goal-type-and-context[Simplified]"),
  (GoalTypeAndContext(Instantiated), "goal-type-and-context[Instantiated]"),
  (GoalTypeAndContext(Normalised), "goal-type-and-context[Normalised]"),
  (
    GoalTypeContextAndInferredType(Simplified),
    "goal-type-context-and-inferred-type[Simplified]",
  ),
  (
    GoalTypeContextAndInferredType(Instantiated),
    "goal-type-context-and-inferred-type[Instantiated]",
  ),
  (
    GoalTypeContextAndInferredType(Normalised),
    "goal-type-context-and-inferred-type[Normalised]",
  ),
  (
    GoalTypeContextAndCheckedType(Simplified),
    "goal-type-context-and-checked-type[Simplified]",
  ),
  (
    GoalTypeContextAndCheckedType(Instantiated),
    "goal-type-context-and-checked-type[Instantiated]",
  ),
  (
    GoalTypeContextAndCheckedType(Normalised),
    "goal-type-context-and-checked-type[Normalised]",
  ),
  (ModuleContents(Simplified), "module-contents[Simplified]"),
  (ModuleContents(Instantiated), "module-contents[Instantiated]"),
  (ModuleContents(Normalised), "module-contents[Normalised]"),
  (ComputeNormalForm(DefaultCompute), "compute-normal-form[DefaultCompute]"),
  (ComputeNormalForm(IgnoreAbstract), "compute-normal-form[IgnoreAbstract]"),
  (
    ComputeNormalForm(UseShowInstance),
    "compute-normal-form[UseShowInstance]",
  ),
  (WhyInScope, "why-in-scope"),
  (Escape, "escape"),
  (InputMethod(Activate), "input-symbol[Activate]"),
  (InputMethod(MoveUp), "input-symbol[MoveUp]"),
  (InputMethod(MoveRight), "input-symbol[MoveRight]"),
  (InputMethod(MoveDown), "input-symbol[MoveDown]"),
  (InputMethod(MoveLeft), "input-symbol[MoveLeft]"),
  (InputMethod(InsertChar("{")), "input-symbol[InsertOpenCurlyBraces]"),
  (InputMethod(InsertChar("(")), "input-symbol[InsertOpenParenthesis]"),
|];

// for human
let toString =
  fun
  | Load => "Load"
  | Quit => "Quit"
  | Restart => "Restart"
  | Compile => "Compile"
  | ToggleDisplayOfImplicitArguments => "Toggle display of hidden arguments"
  | ShowConstraints => "Show constraints"
  | SolveConstraints(normalization) =>
    "Solve constraints " ++ Normalization.toString(normalization)
  | ShowGoals => "Show goals"
  | NextGoal => "Next goal"
  | PreviousGoal => "Previous goal"
  | SearchAbout(normalization) =>
    "Search about " ++ Normalization.toString(normalization)
  | Give => "Give"
  | Refine => "Refine"
  | ElaborateAndGive(normalization) =>
    "Elaborate and give " ++ Normalization.toString(normalization)
  | Auto => "Auto"
  | Case => "Case"
  | HelperFunctionType(normalization) =>
    "Helper function type " ++ Normalization.toString(normalization)
  | InferType(normalization) =>
    "Infer type " ++ Normalization.toString(normalization)
  | Context(normalization) =>
    "Context " ++ Normalization.toString(normalization)
  | GoalType(normalization) =>
    "Goal type " ++ Normalization.toString(normalization)
  | GoalTypeAndContext(normalization) =>
    "Goal type and context " ++ Normalization.toString(normalization)
  | GoalTypeContextAndInferredType(normalization) =>
    "Goal type, context and inferred type "
    ++ Normalization.toString(normalization)
  | GoalTypeContextAndCheckedType(normalization) =>
    "Goal type, context and checked type "
    ++ Normalization.toString(normalization)
  | ModuleContents(normalization) =>
    "Module contents " ++ Normalization.toString(normalization)
  | ComputeNormalForm(DefaultCompute) => "Compute normal form (DefaultCompute)"
  | ComputeNormalForm(IgnoreAbstract) => "Compute normal form (IgnoreAbstract)"
  | ComputeNormalForm(UseShowInstance) => "Compute normal form (UseShowInstance)"
  | WhyInScope => "Why in scope"
  | EventFromView(_) => "Event from the view"
  | Refresh => "Refresh "
  | Escape => "Escape"
  | InputMethod(action) => "Input symbol " ++ InputMethod.toString(action);
