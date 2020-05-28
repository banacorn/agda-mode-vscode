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

type t =
  | Load
  | Quit
  | NextGoal
  | PreviousGoal
  | Auto
  | InferType(Normalization.t)
  | GoalType(Normalization.t)
  | ViewResponse(View.Response.t);

// for registering Keybindings
let names: array((t, string)) = [|
  (Load, "load"),
  (Quit, "quit"),
  (NextGoal, "next-goal"),
  (PreviousGoal, "previous-goal"),
  (Auto, "auto"),
  (InferType(Simplified), "infer-type[Simplified]"),
  (InferType(Instantiated), "infer-type[Instantiated]"),
  (InferType(Normalised), "infer-type[Normalised]"),
  (GoalType(Simplified), "goal-type[Simplified]"),
  (GoalType(Instantiated), "goal-type[Instantiated]"),
  (GoalType(Normalised), "goal-type[Normalised]"),
|];

// for human
let toString =
  fun
  | Load => "Load"
  | Quit => "Quit"
  | NextGoal => "Next Goal"
  | PreviousGoal => "Previous Goal"
  | Auto => "Auto"
  | InferType(Simplified) => "Infer Type (simplified)"
  | InferType(Instantiated) => "Infer Type (instantiated)"
  | InferType(Normalised) => "Infer Type (normalised)"
  | GoalType(Simplified) => "Goal Type (simplified)"
  | GoalType(Instantiated) => "Goal Type (instantiated)"
  | GoalType(Normalised) => "Goal Type (normalised)"
  | ViewResponse(_) => "View Response";