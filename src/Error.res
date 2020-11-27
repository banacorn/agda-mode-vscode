type t =
  | Connection(Connection.Error.t)
  | Parser(Parser.Error.t)
  | OutOfGoal // cursor not in a goal
