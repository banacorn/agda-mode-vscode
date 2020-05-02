module Impl = (Editor: Sig.Editor) => {
  module TaskCommand = Task__Command.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open Belt;

  // run the Tasks
  let run = (state: State.t, tasks: list(Task.t)): Promise.t(unit) => {
    let runTask = (task: Task.t): Promise.t(list(Task.t)) =>
      switch (task) {
      | Task.WithState(callback) => callback(state)
      | DispatchCommand(command) =>
        Js.log2("[ task ][ dispatch command ]", command);
        TaskCommand.dispatch(command)->Promise.resolved;
      | SendRequest(request) =>
        Js.log("[ task ][ send request ]");
        state
        ->State.sendRequest(request)
        ->Promise.map(ev => {[]})
        ->Promise.map(_ => []);
      | Connect =>
        Js.log("[ task ][ connect ]");
        state
        ->State.connect
        ->Promise.tapOk(Js.log2("OK"))
        ->Promise.tapError(Js.log2("Error"))
        ->Promise.map(
            fun
            | Error(_) => []
            | Ok(_) => [],
          );
      // state
      // ->State.sendRequest(request)
      // ->Promise.flatMap(
      //     fun
      //     | Error(error) => {
      //         let (header, body) = Sig.Error.toString(error);
      //         [Task.Display(Error(header), Plain(body))] |> run(state);
      //       }
      //     | Ok(response) => TaskResponse.handle(response) |> run(state),
      //   );
      };

    let rec runEach =
      fun
      | [] => Promise.resolved()
      | [x, ...xs] => {
          runTask(x)->Promise.flatMap(xs' => runEach(List.concat(xs', xs)));
        };
    runEach(tasks);
  };
};