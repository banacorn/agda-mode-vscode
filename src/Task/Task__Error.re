open Error;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  // module State = State.Impl(Editor);
  open! Task;
  // from Error to Tasks
  let handle =
    fun
    | Connection(error) => {
        Js.log(Connection.Error.toString(error));
        [];
      }
    | Parser(error) => {
        Js.log(Parser.Error.toString(error));
        [];
      }
    | _ => [];
};