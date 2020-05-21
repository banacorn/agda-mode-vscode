// from Agda Response to Tasks
module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open! Task;
  open Response;
  open View.Request;

  module DisplayInfo = {
    open Response.DisplayInfo;
    let handle =
      fun
      | AllGoalsWarnings(header, body) => [
          ViewReq(Plain(Plain(header), body)),
        ]
      | _ => [];
  };

  let handle =
    fun
    | DisplayInfo(info) => DisplayInfo.handle(info)
    | _ => [];
};