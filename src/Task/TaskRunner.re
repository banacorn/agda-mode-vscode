module Impl = (Editor: Sig.Editor) => {
  module TaskCommand = Task__Command.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open Belt;
  // run the Tasks
  let rec run = (state: State.t, tasks: list(Task.t)): Promise.t(unit) => {
    let runTask = task =>
      switch (task) {
      | Task.WithState(callback) =>
        callback(state)->Promise.flatMap(run(state))
      | DispatchCommand(command) =>
        Js.log2("[ dispatch command ]", command);
        TaskCommand.dispatch(command) |> run(state);
      | SendRequest(request) =>
        Js.log("[ send request ]");
        Promise.resolved();
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
          let%P () = runTask(x);
          let%P () = runEach(xs);
          Promise.resolved();
        };
    runEach(tasks);
  };
};