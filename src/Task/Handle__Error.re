open Error;

open! Task;
// from Error to Tasks
let handle =
  fun
  | Connection(error) => {
      let (header, body) = Connection.Error.toString(error);
      [display(Error("Connection Error: " ++ header), Plain(body))];
    }
  | Parser(error) => {
      let body = Parser.Error.toString(error);
      [display(Error("Internal Parse Error"), Plain(body))];
    }
  | OutOfGoal => [
      display(
        Error("Out of goal"),
        Plain("Please place the cursor in a goal"),
      ),
    ];
