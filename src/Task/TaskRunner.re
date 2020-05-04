module Impl = (Editor: Sig.Editor) => {
  module TaskCommand = Task__Command.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open Belt;

  let runTask = (state, task: Task.t): Promise.t(list(Task.t)) =>
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

  type t = {
    emitter: Event.t(Task.t),
    listener: unit => unit,
  };

  let destroy = self => {
    self.emitter.destroy();
    self.listener();
  };

  let make = (state: State.t): t => {
    let emitter = Event.make();

    let queue: array(Task.t) = [||];
    // use a semaphore so that there's only one worker executing tasks at a time
    let busy = ref(false);

    let rec runTasksInQueue = () => {
      busy := true;
      switch (Js.Array.shift(queue)) {
      | None =>
        // no more tasks to do, clear the semaphore
        busy := false
      | Some(task) =>
        // execute the task
        runTask(state, task)
        // emit new derived tasks
        ->Promise.map(tasks => tasks->List.forEach(emitter.emit))
        // try to execute the next task
        ->Promise.get(() => runTasksInQueue())
      };
    };

    let listener =
      emitter.on(task => {
        // push the task into the queue
        Js.Array.push(task, queue)->ignore;
        // start executing them
        if (! busy^) {
          runTasksInQueue();
        };
      });

    {emitter, listener};
  };

  let addTask = (runner: t, task: Task.t) => {
    runner.emitter.emit(task);
  };
};