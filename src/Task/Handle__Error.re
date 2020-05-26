open Error;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  open! Task;
  // from Error to Tasks
  let handle =
    fun
    | Connection(error) => {
        let (header, body) = Connection.Error.toString(error);
        [
          ViewReq(Plain(Error("Connection Error: " ++ header), Some(body))),
        ];
      }
    | Parser(error) => {
        let body = Parser.Error.toString(error);
        [ViewReq(Plain(Error("Internal Parse Error"), Some(body)))];
      }
    | Cancelled => [ViewReq(Plain(Error("Query Cancelled"), None))]
    | OutOfGoal => [
        ViewReq(
          Plain(
            Error("Out of goal"),
            Some("Please place the cursor in a goal"),
          ),
        ),
      ];
};