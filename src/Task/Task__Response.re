// from Agda Response to Tasks
module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open! Task;
  open Response;
  open View.Request;

  module DisplayInfo = {
    let handle =
      fun
      | Response.DisplayInfo.CompilationOk => [
          ViewReq(Plain(Success("Compilation Done!"), None)),
        ]
      | Constraints(None) => [
          ViewReq(Plain(Plain("No Constraints"), None)),
        ]
      | Constraints(Some(payload)) => [
          ViewReq(Plain(Plain("Constraints"), Some(payload))),
        ]
      | AllGoalsWarnings(header, body) => [
          ViewReq(Plain(Plain(header), Some(body))),
        ]
      | Time(payload) => [ViewReq(Plain(Plain("Time"), Some(payload)))]
      | Error(payload) => [ViewReq(Plain(Error("Error"), Some(payload)))]
      | Intro(payload) => [ViewReq(Plain(Plain("Intro"), Some(payload)))]
      | Auto(payload) => [ViewReq(Plain(Success("Auto"), Some(payload)))]
      | ModuleContents(payload) => [
          ViewReq(Plain(Plain("Module Contents"), Some(payload))),
        ]
      | SearchAbout(payload) => [
          ViewReq(Plain(Plain("earching about ..."), Some(payload))),
        ]
      | WhyInScope(payload) => [
          ViewReq(Plain(Plain("Scope info"), Some(payload))),
        ]
      | NormalForm(payload) => [
          ViewReq(Plain(Plain("Normal form"), Some(payload))),
        ]
      | GoalType(payload) => [
          ViewReq(Plain(Plain("Normal form"), Some(payload))),
        ]
      | CurrentGoal(payload) => [
          ViewReq(Plain(Plain("Current goal"), Some(payload))),
        ]
      | InferredType(payload) => [
          ViewReq(Plain(Plain("Inferred type"), Some(payload))),
        ]
      | Context(payload) => [
          ViewReq(Plain(Plain("Context"), Some(payload))),
        ]
      | HelperFunction(payload) => [
          ViewReq(Plain(Plain("Helper function"), Some(payload))),
        ]
      | Version(payload) => [
          ViewReq(Plain(Plain("Version"), Some(payload))),
        ];
  };

  let handle =
    fun
    | DisplayInfo(info) => DisplayInfo.handle(info)
    | RunningInfo(_verbosity, message) => [
        ViewReq(Plain(Plain("Type-checking"), Some(message))),
      ]
    | _ => [];
};