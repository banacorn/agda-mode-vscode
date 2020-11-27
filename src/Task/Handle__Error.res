open Error

open! Task
// from Error to Tasks
let handle = x =>
  switch x {
  | Connection(error) =>
    let (header, body) = Connection.Error.toString(error)
    list{display(Error("Connection Error: " ++ header), Plain(body))}
  | Parser(error) =>
    let body = Parser.Error.toString(error)
    list{display(Error("Internal Parse Error"), Plain(body))}
  | OutOfGoal => list{display(Error("Out of goal"), Plain("Please place the cursor in a goal"))}
  }
