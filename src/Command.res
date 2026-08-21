module Normalization = {
  type t =
    | AsIs
    | Simplified
    | Instantiated
    | Normalised
    | HeadNormal

  // for Agda
  let encode = x =>
    switch x {
    | AsIs => "AsIs"
    | Simplified => "Simplified"
    | Instantiated => "Instantiated"
    | Normalised => "Normalised"
    | HeadNormal => "HeadNormal"
    }

  // for human
  let toString = x =>
    switch x {
    | AsIs => "(returned as is)"
    | Simplified => "(simplified)"
    | Instantiated => "(neither explicitly normalised nor simplified)"
    | Normalised => "(normalised)"
    | HeadNormal => "(head normalized)"
    }
}

module ComputeMode = {
  type t =
    | DefaultCompute
    | IgnoreAbstract
    | UseShowInstance

  // for Agda
  let encode = x =>
    switch x {
    | DefaultCompute => "DefaultCompute"
    | IgnoreAbstract => "IgnoreAbstract"
    | UseShowInstance => "UseShowInstance"
    }

  let ignoreAbstract = x =>
    switch x {
    | DefaultCompute => false
    | IgnoreAbstract => true
    | UseShowInstance => true
    }
}

module InputMethod = {
  type t =
    | Activate
    | InsertChar(string)
    | BrowseUp
    | BrowseRight
    | BrowseDown
    | BrowseLeft

  let toString = x =>
    switch x {
    | Activate => "Activate"
    | InsertChar(char) => "InsertChar '" ++ (char ++ "'")
    | BrowseUp => "BrowseUp"
    | BrowseRight => "BrowseRight"
    | BrowseDown => "BrowseDown"
    | BrowseLeft => "BrowseLeft"
    }
}

type t =
  | Load
  | Quit
  | Restart
  | Refresh
  | Compile
  | ToggleDisplayOfImplicitArguments
  | ToggleDisplayOfIrrelevantArguments
  | ShowConstraints(Normalization.t)
  | SolveConstraints(Normalization.t) // agda2-maybe-normalised-toplevel-asis-noprompt
  | ShowGoals(Normalization.t) // agda2-maybe-normalised-toplevel-asis-noprompt
  | NextGoal
  | PreviousGoal
  | SearchAbout(Normalization.t) // agda2-maybe-normalised
  | Give
  | Refine
  | ElaborateAndGive(Normalization.t) // agda2-maybe-normalised
  | Auto(Normalization.t) // agda2-maybe-normalised-asis
  | Case
  | HelperFunctionType(Normalization.t) // agda2-maybe-normalised-asis
  | InferType(Normalization.t) // agda2-maybe-normalised
  | Context(Normalization.t) // agda2-maybe-normalised
  | GoalType(Normalization.t) // agda2-maybe-normalised
  | GoalTypeAndContext(Normalization.t) // agda2-maybe-normalised
  | EventFromView(View.EventFromView.t)
  | GoalTypeContextAndInferredType(Normalization.t) // agda2-maybe-normalised
  | GoalTypeContextAndCheckedType(Normalization.t) // agda2-maybe-normalised
  | ModuleContents(Normalization.t) // agda2-maybe-normalised
  | ComputeNormalForm(ComputeMode.t)
  | WhyInScope
  | SwitchAgdaVersion
  | Escape
  | InputMethod(InputMethod.t)
  | LookupSymbol
  | OpenDebugBuffer

// for registering Keybindings
let names: array<(t, string)> = [
  (Load, "load"),
  (Quit, "quit"),
  (Restart, "restart"),
  (Compile, "compile"),
  (ToggleDisplayOfImplicitArguments, "toggle-display-of-implicit-arguments"),
  (ToggleDisplayOfIrrelevantArguments, "toggle-display-of-irrelevant-arguments"),
  (ShowConstraints(AsIs), "show-constraints[AsIs]"),
  (ShowConstraints(Simplified), "show-constraints[Simplified]"),
  (ShowConstraints(Normalised), "show-constraints[Normalised]"),
  (ShowConstraints(HeadNormal), "show-constraints[HeadNormal]"),
  (SolveConstraints(AsIs), "solve-constraints[AsIs]"),
  (SolveConstraints(Simplified), "solve-constraints[Simplified]"),
  (SolveConstraints(Normalised), "solve-constraints[Normalised]"),
  (SolveConstraints(HeadNormal), "solve-constraints[HeadNormal]"),
  (ShowGoals(AsIs), "show-goals[AsIs]"),
  (ShowGoals(Simplified), "show-goals[Simplified]"),
  (ShowGoals(Normalised), "show-goals[Normalised]"),
  (ShowGoals(HeadNormal), "show-goals[HeadNormal]"),
  (NextGoal, "next-goal"),
  (PreviousGoal, "previous-goal"),
  (SearchAbout(Simplified), "search-about[Simplified]"),
  (SearchAbout(Instantiated), "search-about[Instantiated]"),
  (SearchAbout(Normalised), "search-about[Normalised]"),
  (SearchAbout(HeadNormal), "search-about[HeadNormal]"),
  (Give, "give"),
  (Refine, "refine"),
  (ElaborateAndGive(Simplified), "elaborate-and-give[Simplified]"),
  (ElaborateAndGive(Instantiated), "elaborate-and-give[Instantiated]"),
  (ElaborateAndGive(Normalised), "elaborate-and-give[Normalised]"),
  (ElaborateAndGive(HeadNormal), "elaborate-and-give[HeadNormal]"),
  (Auto(AsIs), "auto[AsIs]"),
  (Auto(Simplified), "auto[Simplified]"),
  (Auto(Normalised), "auto[Normalised]"),
  (Auto(HeadNormal), "auto[HeadNormal]"),
  (Case, "case"),
  (HelperFunctionType(AsIs), "helper-function-type[AsIs]"),
  (HelperFunctionType(Simplified), "helper-function-type[Simplified]"),
  (HelperFunctionType(Normalised), "helper-function-type[Normalised]"),
  (HelperFunctionType(HeadNormal), "helper-function-type[HeadNormal]"),
  (InferType(Simplified), "infer-type[Simplified]"),
  (InferType(Instantiated), "infer-type[Instantiated]"),
  (InferType(Normalised), "infer-type[Normalised]"),
  (InferType(HeadNormal), "infer-type[HeadNormal]"),
  (Context(Simplified), "context[Simplified]"),
  (Context(Instantiated), "context[Instantiated]"),
  (Context(Normalised), "context[Normalised]"),
  (Context(HeadNormal), "context[HeadNormal]"),
  (GoalType(Simplified), "goal-type[Simplified]"),
  (GoalType(Instantiated), "goal-type[Instantiated]"),
  (GoalType(Normalised), "goal-type[Normalised]"),
  (GoalType(HeadNormal), "goal-type[HeadNormal]"),
  (GoalTypeAndContext(Simplified), "goal-type-and-context[Simplified]"),
  (GoalTypeAndContext(Instantiated), "goal-type-and-context[Instantiated]"),
  (GoalTypeAndContext(Normalised), "goal-type-and-context[Normalised]"),
  (GoalTypeAndContext(HeadNormal), "goal-type-and-context[HeadNormal]"),
  (GoalTypeContextAndInferredType(Simplified), "goal-type-context-and-inferred-type[Simplified]"),
  (
    GoalTypeContextAndInferredType(Instantiated),
    "goal-type-context-and-inferred-type[Instantiated]",
  ),
  (GoalTypeContextAndInferredType(Normalised), "goal-type-context-and-inferred-type[Normalised]"),
  (GoalTypeContextAndInferredType(HeadNormal), "goal-type-context-and-inferred-type[HeadNormal]"),
  (GoalTypeContextAndCheckedType(Simplified), "goal-type-context-and-checked-type[Simplified]"),
  (GoalTypeContextAndCheckedType(Instantiated), "goal-type-context-and-checked-type[Instantiated]"),
  (GoalTypeContextAndCheckedType(Normalised), "goal-type-context-and-checked-type[Normalised]"),
  (GoalTypeContextAndCheckedType(HeadNormal), "goal-type-context-and-checked-type[HeadNormal]"),
  (ModuleContents(Simplified), "module-contents[Simplified]"),
  (ModuleContents(Instantiated), "module-contents[Instantiated]"),
  (ModuleContents(Normalised), "module-contents[Normalised]"),
  (ModuleContents(HeadNormal), "module-contents[HeadNormal]"),
  (ComputeNormalForm(DefaultCompute), "compute-normal-form[DefaultCompute]"),
  (ComputeNormalForm(IgnoreAbstract), "compute-normal-form[IgnoreAbstract]"),
  (ComputeNormalForm(UseShowInstance), "compute-normal-form[UseShowInstance]"),
  (WhyInScope, "why-in-scope"),
  (SwitchAgdaVersion, "switch-agda-version"),
  (Escape, "escape"),
  (InputMethod(Activate), "input-symbol[Activate]"),
  (InputMethod(BrowseUp), "input-symbol[BrowseUp]"),
  (InputMethod(BrowseRight), "input-symbol[BrowseRight]"),
  (InputMethod(BrowseDown), "input-symbol[BrowseDown]"),
  (InputMethod(BrowseLeft), "input-symbol[BrowseLeft]"),
  (InputMethod(InsertChar("{")), "input-symbol[InsertOpenCurlyBraces]"),
  (InputMethod(InsertChar("(")), "input-symbol[InsertOpenParenthesis]"),
  (LookupSymbol, "lookup-symbol"),
  (OpenDebugBuffer, "open-debug-buffer"),
]

// like `names`
let toKeybinding = x =>
  switch x {
  | Load => "load"
  | Quit => "quit"
  | Restart => "restart"
  | Refresh => "refresh"
  | Compile => "compile"
  | ToggleDisplayOfImplicitArguments => "toggle-display-of-implicit-arguments"
  | ToggleDisplayOfIrrelevantArguments => "toggle-display-of-irrelevant-arguments"
  | ShowConstraints(normalization) => "show-constraints[" ++ Normalization.encode(normalization) ++ "]"
  | SolveConstraints(normalization) =>
    "solve-constraints[" ++ Normalization.encode(normalization) ++ "]"
  | ShowGoals(normalization) => "show-goals[" ++ Normalization.encode(normalization) ++ "]"
  | NextGoal => "next-goal"
  | PreviousGoal => "previous-goal"
  | SearchAbout(normalization) => "search-about[" ++ Normalization.encode(normalization) ++ "]"
  | Give => "give"
  | Refine => "refine"
  | ElaborateAndGive(normalization) =>
    "elaborate-and-give[" ++ Normalization.encode(normalization) ++ "]"
  | Auto(normalization) => "auto[" ++ Normalization.encode(normalization) ++ "]"
  | Case => "case"
  | HelperFunctionType(normalization) =>
    "helper-function-type[" ++ Normalization.encode(normalization) ++ "]"
  | InferType(normalization) => "infer-type[" ++ Normalization.encode(normalization) ++ "]"
  | Context(normalization) => "context[" ++ Normalization.encode(normalization) ++ "]"
  | GoalType(normalization) => "goal-type[" ++ Normalization.encode(normalization) ++ "]"
  | GoalTypeAndContext(normalization) =>
    "goal-type-and-context[" ++ Normalization.encode(normalization) ++ "]"
  | GoalTypeContextAndInferredType(normalization) =>
    "goal-type-context-and-inferred-type[" ++ Normalization.encode(normalization) ++ "]"
  | GoalTypeContextAndCheckedType(normalization) =>
    "goal-type-context-and-checked-type[" ++ Normalization.encode(normalization) ++ "]"
  | ModuleContents(normalization) =>
    "module-contents[" ++ Normalization.encode(normalization) ++ "]"
  | ComputeNormalForm(mode) => "compute-normal-form[" ++ ComputeMode.encode(mode) ++ "]"
  | WhyInScope => "why-in-scope"
  | SwitchAgdaVersion => "switch-agda-version"
  | Escape => "escape"
  | InputMethod(action) =>
    switch action {
    | InputMethod.Activate => "input-symbol[Activate]"
    | InputMethod.BrowseUp => "input-symbol[BrowseUp]"
    | InputMethod.BrowseRight => "input-symbol[BrowseRight]"
    | InputMethod.BrowseDown => "input-symbol[BrowseDown]"
    | InputMethod.BrowseLeft => "input-symbol[BrowseLeft]"
    | InputMethod.InsertChar(char) =>
      switch char {
      | "{" => "input-symbol[InsertOpenCurlyBraces]"
      | "(" => "input-symbol[InsertOpenParenthesis]"
      | _ => "input-symbol[" ++ char ++ "]"
      }
    }
  | LookupSymbol => "lookup-symbol"
  | OpenDebugBuffer => "open-debug-buffer"
  | EventFromView(event) => "event-from-view[" ++ View.EventFromView.toString(event) ++ "]"
  }

// for human
let toString = x =>
  switch x {
  | Load => "Load"
  | Quit => "Quit"
  | Restart => "Restart"
  | Compile => "Compile"
  | ToggleDisplayOfImplicitArguments => "Toggle display of hidden arguments"
  | ToggleDisplayOfIrrelevantArguments => "Toggle display of irrelevant arguments"
  | ShowConstraints(normalization) => "Show constraints" ++ Normalization.toString(normalization)
  | SolveConstraints(normalization) => "Solve constraints " ++ Normalization.toString(normalization)
  | ShowGoals(normalization) => "Show goals" ++ Normalization.toString(normalization)
  | NextGoal => "Next goal"
  | PreviousGoal => "Previous goal"
  | SearchAbout(normalization) => "Search about " ++ Normalization.toString(normalization)
  | Give => "Give"
  | Refine => "Refine"
  | ElaborateAndGive(normalization) =>
    "Elaborate and give " ++ Normalization.toString(normalization)
  | Auto(normalization) => "Auto " ++ Normalization.toString(normalization)
  | Case => "Case"
  | HelperFunctionType(normalization) =>
    "Helper function type " ++ Normalization.toString(normalization)
  | InferType(normalization) => "Infer type " ++ Normalization.toString(normalization)
  | Context(normalization) => "Context " ++ Normalization.toString(normalization)
  | GoalType(normalization) => "Goal type " ++ Normalization.toString(normalization)
  | GoalTypeAndContext(normalization) =>
    "Goal type and context " ++ Normalization.toString(normalization)
  | GoalTypeContextAndInferredType(normalization) =>
    "Goal type, context and inferred type " ++ Normalization.toString(normalization)
  | GoalTypeContextAndCheckedType(normalization) =>
    "Goal type, context and checked type " ++ Normalization.toString(normalization)
  | ModuleContents(normalization) => "Module contents " ++ Normalization.toString(normalization)
  | ComputeNormalForm(DefaultCompute) => "Compute normal form (DefaultCompute)"
  | ComputeNormalForm(IgnoreAbstract) => "Compute normal form (IgnoreAbstract)"
  | ComputeNormalForm(UseShowInstance) => "Compute normal form (UseShowInstance)"
  | WhyInScope => "Why in scope"
  | SwitchAgdaVersion => "Switch to a different Agda version"
  | EventFromView(event) => "Event from the view (" ++ View.EventFromView.toString(event) ++ ")"
  | Refresh => "Refresh "
  | Escape => "Escape"
  | InputMethod(action) => "Input symbol " ++ InputMethod.toString(action)
  | LookupSymbol => "Lookup Unicode symbol input sequence"
  | OpenDebugBuffer => "Open debug buffer"
  }

/*
  Issue #335: these commands are answered from the state Agda built during the
  last load, so they are only meaningful while the file still says what Agda
  was told it says.

  Once the buffer has been edited we cannot know what changed without
  typechecking again -- an edit anywhere above a goal can change that goal's
  type, and goal-indexed requests like `Cmd_goal_type_context` carry an index
  rather than a range, so Agda will happily answer them from its own stale
  interaction points. The answer looks perfectly plausible and is wrong.
  Dispatching a load first is the only way to make the answer true.

  Deliberately excluded, and why:

    Load, Restart          reload anyway
    Quit                   tears the session down
    Refresh                redraws the view, asks Agda nothing
    NextGoal, PreviousGoal move the cursor; goal positions are already rebased
                           against edits by `Goals.scanAllGoals`
    ToggleDisplayOf*       set a display preference rather than report on the file
    SwitchAgdaVersion      reconnects, which loads
    Escape, InputMethod,
    LookupSymbol,
    OpenDebugBuffer,
    EventFromView          never consult Agda's view of the file
 */
let requiresUpToDateLoad = x =>
  switch x {
  | Compile
  | ShowConstraints(_)
  | SolveConstraints(_)
  | ShowGoals(_)
  | SearchAbout(_)
  | Give
  | Refine
  | ElaborateAndGive(_)
  | Auto(_)
  | Case
  | HelperFunctionType(_)
  | InferType(_)
  | Context(_)
  | GoalType(_)
  | GoalTypeAndContext(_)
  | GoalTypeContextAndInferredType(_)
  | GoalTypeContextAndCheckedType(_)
  | ModuleContents(_)
  | ComputeNormalForm(_)
  | WhyInScope => true
  | Load
  | Quit
  | Restart
  | Refresh
  | NextGoal
  | PreviousGoal
  | ToggleDisplayOfImplicitArguments
  | ToggleDisplayOfIrrelevantArguments
  | SwitchAgdaVersion
  | Escape
  | InputMethod(_)
  | LookupSymbol
  | OpenDebugBuffer
  | EventFromView(_) => false
  }
