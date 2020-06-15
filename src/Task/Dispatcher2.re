open Belt;

module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Handle__Error.Impl(Editor);
  // module ViewHandler = Handle__View.Impl(Editor);

  module GoalHandler = Handle__Goal.Impl(Editor);
  module CommandHandler = Handle__Command.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module Dispatcher = Dispatcher.Impl(Editor);
  module Task = Task.Impl(Editor);
  // module State = State.Impl(Editor);
  // module Request = Request.Impl(Editor);
  // type t = Runner.t(Command.t);
  open! Task;

  type blockedQueue =
    | BlockedByAgda(list(Task.t))
    | BlockedByView(list(Task.t));

  type t = {
    runner: Runner3.t(Task.t),
    mutable blockedQueues: list(blockedQueue),
  };

  let dispatchCommand = (self, state, command) => {
    module CommandHandler = Handle__Command.Impl(Editor);
    let tasks = CommandHandler.handle(command);
    self.runner->Runner3.pushAndRun(tasks);
  };
  let make = state => {
    let blockedQueues = ref([]);

    let agdaIsOccupied = queues =>
      List.length(queues) != 0
      && queues->List.some(
           fun
           | BlockedByAgda(_) => true
           | _ => false,
         );

    let viewIsOccupied = queues =>
      List.length(queues) != 0
      && queues->List.some(
           fun
           | BlockedByView(_) => true
           | _ => false,
         );

    let rec releaseAgda =
      fun
      | [] => (None, [])
      | [BlockedByAgda(queue), ...others] => (Some(queue), others)
      | [BlockedByView(other), ...others] => {
          let (queue, others) = releaseAgda(others);
          switch (queue) {
          | Some(queue) => (
              None,
              [BlockedByView(List.concat(other, queue)), ...others],
            )
          | None => (None, [BlockedByView(other), ...others])
          };
        };

    let rec releaseView =
      fun
      | [] => (None, [])
      | [BlockedByView(queue), ...others] => (Some(queue), others)
      | [BlockedByAgda(other), ...others] => {
          let (queue, others) = releaseView(others);
          switch (queue) {
          | Some(queue) => (
              None,
              [BlockedByAgda(List.concat(other, queue)), ...others],
            )
          | None => (None, [BlockedByAgda(other), ...others])
          };
        };

    let runner = Runner3.make();

    let printStatus = (task, runner: Runner3.t(Task.t), queues) => {
      Js.log(
        "\nTask: "
        ++ Task.toString(task)
        ++ "\nRunner: "
        ++ Util.Pretty.array(Array.map(runner.queue, Task.toString))
        ++ "\n===============================",
      );

      queues->List.forEach(
        fun
        | BlockedByAgda(queue) =>
          Js.log(
            "Agda " ++ Util.Pretty.list(List.map(queue, Task.toString)),
          )
        | BlockedByView(queue) =>
          Js.log(
            "View " ++ Util.Pretty.list(List.map(queue, Task.toString)),
          ),
      );
    };

    let classifyTask = task => {
      printStatus(task, runner, blockedQueues^);
      switch (task) {
      | ViewReq(request, callback) =>
        if (viewIsOccupied(blockedQueues^)) {
          Js.log("View blocked");
          Promise.resolved();
        } else {
          Js.log("View not blocked");
          state
          ->State.sendRequestToView(request)
          ->Promise.map(response => {
              // empty the current Runner and move the content to `blockedQueues`
              let queue = Runner3.empty(runner)->List.fromArray;
              blockedQueues := [BlockedByView(queue), ...blockedQueues^];

              printStatus(task, runner, blockedQueues^);
              let tasks = callback(response);
              runner->Runner3.pushAndRun(tasks);
              Js.log("View unblocked");
              let (queue, queues) = releaseView(blockedQueues^);
              blockedQueues := queues;
              switch (queue) {
              | None => ()
              | Some(queue) => runner->Runner3.pushAndRun(queue)
              };
            });
        }
      | SendRequest(request) =>
        if (agdaIsOccupied(blockedQueues^)) {
          Js.log("Agda blocked");
          Promise.resolved();
        } else {
          Js.log("Agda not blocked");
          // empty the current Runner and move the content to `blockedQueues`
          let queue = Runner3.empty(runner)->List.fromArray;
          blockedQueues := [BlockedByAgda(queue), ...blockedQueues^];
          // issue request
          Dispatcher.sendAgdaRequest(
            tasks => {
              printStatus(task, runner, blockedQueues^);
              runner->Runner3.pushAndRun(tasks);
              Promise.resolved();
            },
            state,
            request,
          )
          ->Promise.tap(() => {
              Js.log("Agda unblocked");
              let (queue, queues) = releaseAgda(blockedQueues^);
              blockedQueues := queues;
              switch (queue) {
              | None => ()
              | Some(queue) => runner->Runner3.pushAndRun(queue)
              };
            })
          ->ignore;
          Promise.resolved();
        }
      | WithState(callback) =>
        callback(state)
        ->Promise.map(tasks => {runner->Runner3.pushAndRun(tasks)})

      | Terminate => State.destroy(state)
      | Goal(action) =>
        let tasks = GoalHandler.handle(action);
        runner->Runner3.pushAndRun(tasks);
        Promise.resolved();
      | ViewEvent(event) =>
        let tasks =
          switch (event) {
          | Initialized => []
          | Destroyed => [Task.Terminate]
          };
        runner->Runner3.pushAndRun(tasks);
        Promise.resolved();
      | Error(error) =>
        let tasks = ErrorHandler.handle(error);
        runner->Runner3.pushAndRun(tasks);
        Promise.resolved();
      | Debug(message) =>
        let tasks = [Task.displayWarning("Debug", Some(message))];
        runner->Runner3.pushAndRun(tasks);
        Promise.resolved();
      };
    };

    Runner3.setup(runner, classifyTask);
    {runner, blockedQueues: []};
  };

  let interrupt = (self, command) => Promise.resolved();

  let destroy = _ => ();
};