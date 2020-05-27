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
  | GoalType(Normalization.t)
  | ViewResponse(View.Response.t);

// for registering Keybindings
let names: array((t, string)) = [|
  (Load, "load"),
  (Quit, "quit"),
  (NextGoal, "next-goal"),
  (PreviousGoal, "previous-goal"),
  (Auto, "auto"),
  (GoalType(Simplified), "goal-type[Simplified]"),
  (GoalType(Instantiated), "goal-type[Instantiated]"),
  (GoalType(Normalised), "goal-type[Normalised]"),
|];

// for human
let toString =
  fun
  | Load => "Load"
  | Quit => "Quit"
  | NextGoal => "NextGoal"
  | PreviousGoal => "PreviousGoal"
  | Auto => "Auto"
  | GoalType(Simplified) => "GoalType[Simplified]"
  | GoalType(Instantiated) => "GoalType[Instantiated]"
  | GoalType(Normalised) => "GoalType[Normalised]"
  | ViewResponse(_) => "ViewResponse";