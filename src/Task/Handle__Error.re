open Error;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  open! Task;
  // from Error to Tasks
  let handle =
    fun
    | Connection(error) => {
        let (header, body) = Connection.Error.toString(error);
        [displayError("Connection Error: " ++ header, Some(body))];
      }
    | Parser(error) => {
        let body = Parser.Error.toString(error);
        [displayError("Internal Parse Error", Some(body))];
      }
    | Cancelled => [displayError("Query Cancelled", None)]
    | OutOfGoal => [
        displayError(
          "Out of goal",
          Some("Please place the cursor in a goal"),
        ),
      ]
    | NoTextSelectedAndOutOfGoal => [
        displayError(
          "No text selected and cursor out of goal",
          Some("Please select some text or place the cursor in a goal"),
        ),
      ];
};