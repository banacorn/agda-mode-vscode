// from Agda Response to Tasks
module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  open! Task;
  open Response;
  module DisplayInfo = {
    let handle =
      fun
      | Response.DisplayInfo.CompilationOk => [
          displaySuccess("Compilation Done!", None),
        ]
      | Constraints(None) => [display("No Constraints", None)]
      | Constraints(Some(payload)) => [
          display("Constraints", Some(payload)),
        ]
      | AllGoalsWarnings(header, body) => [display(header, Some(body))]
      | Time(payload) => [display("Time", Some(payload))]
      | Error(payload) => [displayError("Error", Some(payload))]
      | Intro(payload) => [display("Intro", Some(payload))]
      | Auto(payload) => [displaySuccess("Auto", Some(payload))]
      | ModuleContents(payload) => [
          display("Module Contents", Some(payload)),
        ]
      | SearchAbout(payload) => [
          display("Searching about ...", Some(payload)),
        ]
      | WhyInScope(payload) => [display("Scope info", Some(payload))]
      | NormalForm(payload) => [display("Normal form", Some(payload))]
      | GoalType(payload) => [display("Goal Type", Some(payload))]
      | CurrentGoal(payload) => [display("Current goal", Some(payload))]
      | InferredType(payload) => [display("Inferred type", Some(payload))]
      | Context(payload) => [display("Context", Some(payload))]
      | HelperFunction(payload) => [
          display("Helper function", Some(payload)),
        ]
      | Version(payload) => [display("Version", Some(payload))];
  };

  let handle =
    fun
    | DisplayInfo(info) => DisplayInfo.handle(info)
    | RunningInfo(_verbosity, message) => [
        display("Type-checking", Some(message)),
      ]
    | InteractionPoints(indices) => [Goal(Instantiate(indices))]
    | _ => [];
};