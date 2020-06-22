type t =
  | Connection(Connection.Error.t)
  | Parser(Parser.Error.t)
  | Cancelled // never makes its way to Agda
  | OutOfGoal // cursor not in a goal
  | NoTextSelectedAndOutOfGoal;

();