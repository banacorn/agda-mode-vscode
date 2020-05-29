// from View Response to Tasks
module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  open! Task;
  open View.Response;

  let handle =
    fun
    | Initialized => []
    | Destroyed => [Terminate]
    | InquiryResult(_) => [];
};