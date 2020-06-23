module Normalization = {
  type t =
    | Simplified
    | Instantiated
    | Normalised;

  let toString =
    fun
    | Simplified => "Simplified"
    | Instantiated => "Instantiated"
    | Normalised => "Normalised";
};

module ComputeMode = {
  type t =
    | DefaultCompute
    | IgnoreAbstract
    | UseShowInstance;

  let toString =
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
  | NextGoal
  | PreviousGoal
  | Give
  | Refine
  | Auto
  | Case
  | InferType(Normalization.t)
  | GoalType(Normalization.t)
  | GoalTypeAndContext(Normalization.t)
  | EventFromView(View.EventFromView.t)
  | ModuleContents(Normalization.t)
  | ComputeNormalForm(ComputeMode.t)
  | WhyInScope
  | Escape
  | InputMethod(InputMethod.t);

// for registering Keybindings
let names: array((t, string)) = [|
  (Load, "load"),
  (Quit, "quit"),
  (NextGoal, "next-goal"),
  (PreviousGoal, "previous-goal"),
  (Give, "give"),
  (Refine, "refine"),
  (Auto, "auto"),
  (Case, "case"),
  (InferType(Simplified), "infer-type[Simplified]"),
  (InferType(Instantiated), "infer-type[Instantiated]"),
  (InferType(Normalised), "infer-type[Normalised]"),
  (GoalType(Simplified), "goal-type[Simplified]"),
  (GoalType(Instantiated), "goal-type[Instantiated]"),
  (GoalType(Normalised), "goal-type[Normalised]"),
  (GoalTypeAndContext(Simplified), "goal-type-and-context[Simplified]"),
  (GoalTypeAndContext(Instantiated), "goal-type-and-context[Instantiated]"),
  (GoalTypeAndContext(Normalised), "goal-type-and-context[Normalised]"),
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
|];

// for human
let toString =
  fun
  | Load => "Load"
  | Quit => "Quit"
  | NextGoal => "Next goal"
  | PreviousGoal => "Previous goal"
  | Give => "Give"
  | Refine => "Refine"
  | Auto => "Auto"
  | Case => "Case"
  | InferType(normalization) =>
    "Infer type (" ++ Normalization.toString(normalization) ++ ")"
  | GoalType(normalization) =>
    "Goal type (" ++ Normalization.toString(normalization) ++ ")"
  | GoalTypeAndContext(normalization) =>
    "Goal type and context (" ++ Normalization.toString(normalization) ++ ")"
  | ModuleContents(normalization) =>
    "Module contents (" ++ Normalization.toString(normalization) ++ ")"
  | ComputeNormalForm(DefaultCompute) => "Compute normal form (DefaultCompute)"
  | ComputeNormalForm(IgnoreAbstract) => "Compute normal form (IgnoreAbstract)"
  | ComputeNormalForm(UseShowInstance) => "Compute normal form (UseShowInstance)"
  | WhyInScope => "Why in scope"
  | EventFromView(_) => "Event from the view"
  | Escape => "Escape"
  | InputMethod(action) => "Input symbol " ++ InputMethod.toString(action);