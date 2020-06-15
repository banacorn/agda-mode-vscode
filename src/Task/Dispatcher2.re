open Belt;

module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Handle__Error.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);
  module CommandHandler = Handle__Command.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module Dispatcher = Dispatcher.Impl(Editor);
  module Task = Task.Impl(Editor);
  open! Task;

  type blockedBy =
    | Agda
    | View;

  type t = {
    runner: Runner3.t(Task.t),
    mutable blockedQueues: list((blockedBy, list(Task.t))),
  };

  let dispatchCommand = (self, state, command) => {
    module CommandHandler = Handle__Command.Impl(Editor);
    let tasks = CommandHandler.handle(command);
    self.runner->Runner3.pushAndRun(tasks);
  };
  let make = state => {
    let blockedQueues = ref([]);

    // scan through a list of Queues and see if it's blocked by some resource
    let blockedBy = (kind, queues) =>
      List.length(queues) != 0
      && queues->List.some(((kind', _)) => kind == kind');

    // scan through a list of Queues and if there's a queue blocked by some resource
    // if that blocked queue has the highest priority (placed as the first queue)
    //    then return the queue, and delete it from the queues
    //    else return nothing, merge the blocked queue with the queue before it
    let rec unblock = kind =>
      fun
      | [] => (None, [])
      | [(kind', x), ...xs] =>
        if (kind == kind') {
          (Some(x), xs);
        } else {
          let (queue, others) = unblock(kind, xs);
          switch (queue) {
          | Some(queue) => (
              None,
              [(kind', List.concat(x, queue)), ...others],
            )
          | None => (None, [(kind', x), ...others])
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
        | (Agda, queue) =>
          Js.log(
            "Agda " ++ Util.Pretty.list(List.map(queue, Task.toString)),
          )
        | (View, queue) =>
          Js.log(
            "View " ++ Util.Pretty.list(List.map(queue, Task.toString)),
          ),
      );
    };

    let classifyTask = task => {
      printStatus(task, runner, blockedQueues^);
      switch (task) {
      | ViewReq(request, callback) =>
        if (blockedBy(View, blockedQueues^)) {
          Js.log("View blocked");
          Promise.resolved();
        } else {
          Js.log("View not blocked");
          state
          ->State.sendRequestToView(request)
          ->Promise.map(response => {
              // empty the current Runner and move the content to `blockedQueues`
              let queue = Runner3.empty(runner)->List.fromArray;
              blockedQueues := [(View, queue), ...blockedQueues^];

              printStatus(task, runner, blockedQueues^);
              let tasks = callback(response);
              runner->Runner3.pushAndRun(tasks);
              Js.log("View unblocked");
              let (queue, queues) = unblock(View, blockedQueues^);
              blockedQueues := queues;
              switch (queue) {
              | None => ()
              | Some(queue) => runner->Runner3.pushAndRun(queue)
              };
            });
        }
      | SendRequest(request) =>
        if (blockedBy(Agda, blockedQueues^)) {
          Js.log("Agda blocked");
          Promise.resolved();
        } else {
          Js.log("Agda not blocked");
          // empty the current Runner and move the content to `blockedQueues`
          let queue = Runner3.empty(runner)->List.fromArray;
          blockedQueues := [(Agda, queue), ...blockedQueues^];
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
              let (queue, queues) = unblock(Agda, blockedQueues^);
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