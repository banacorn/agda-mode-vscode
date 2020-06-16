open Belt;

module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Handle__Error.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);
  module CommandHandler = Handle__Command.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  open! Task;

  type blockedBy =
    | Agda
    | View;

  let sendAgdaRequest = (runTasks, state, req) => {
    // this promise get resolved after the request to Agda is completed
    let (promise, resolve) = Promise.pending();
    let handle = ref(None);
    let handler: result(Connection.response, Connection.Error.t) => unit =
      fun
      | Error(error) => {
          let tasks = ErrorHandler.handle(Error.Connection(error));
          runTasks(tasks);
        }
      | Ok(Parser.Incr.Event.Yield(Error(error))) => {
          let tasks = ErrorHandler.handle(Error.Parser(error));
          runTasks(tasks);
        }
      | Ok(Yield(Ok(response))) => {
          Js.log(">>> " ++ Response.toString(response));
          let tasks = ResponseHandler.handle(response);
          runTasks(tasks);
        }
      | Ok(Stop) => {
          Js.log(">>| ");
          resolve();
        };

    state
    ->State.sendRequestToAgda(req)
    ->Promise.flatMap(
        fun
        | Ok(connection) => {
            handle := Some(connection.Connection.emitter.on(handler));
            promise;
          }
        | Error(error) => {
            let tasks = ErrorHandler.handle(error);
            runTasks(tasks);
            promise;
          },
      )
    ->Promise.tap(() => (handle^)->Option.forEach(f => f()));
  };

  type t = {
    runner: Runner.t(Task.t),
    mutable blockedQueues: list((blockedBy, list(Task.t))),
  };

  // empty the current Runner and move the content to `blockedQueues`
  let acquire = (self, resource) => {
    let queue = Runner.empty(self.runner)->List.fromArray;
    self.blockedQueues = [(resource, queue), ...self.blockedQueues];
  };

  let onResponse = (self, tasks) => {
    self.runner->Runner.pushAndRun(tasks);
  };

  let release = (self, resource) => {
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

    let (queue, queues) = unblock(resource, self.blockedQueues);
    self.blockedQueues = queues;
    switch (queue) {
    | None => ()
    | Some(queue) => self.runner->Runner.pushAndRun(queue)
    };
  };
  let toString = (self, task) => {
    Js.log(
      "\nTask: "
      ++ Task.toString(task)
      ++ "\nRunner: "
      ++ Util.Pretty.array(Array.map(self.runner.queue, Task.toString))
      ++ "\n===============================",
    );

    self.blockedQueues
    ->List.forEach(
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
  // scan through a list of Queues and see if it's blocked by some resource
  let blockedBy = (self, resource) =>
    List.length(self.blockedQueues) != 0
    && self.blockedQueues
       ->List.some(((resource', _)) => resource == resource');

  let make = state => {
    let self = {runner: Runner.make(), blockedQueues: []};

    let classifyTask = task => {
      toString(self, task);
      switch (task) {
      | ViewReq(request, callback) =>
        if (blockedBy(self, View)) {
          Promise.resolved();
        } else {
          acquire(self, View);
          state
          ->State.sendRequestToView(request)
          ->Promise.map(response => {
              onResponse(self, callback(response));
              release(self, View);
            });
        }
      | SendRequest(request) =>
        if (blockedBy(self, Agda)) {
          Promise.resolved();
        } else {
          acquire(self, Agda);
          // issue request
          sendAgdaRequest(onResponse(self), state, request)
          ->Promise.get(() => {release(self, Agda)});
          // NOTE: early return before `sendAgdaRequest` resolved
          Promise.resolved();
        }
      | WithState(callback) =>
        callback(state)
        ->Promise.map(tasks => {self.runner->Runner.pushAndRun(tasks)})

      | Terminate => State.destroy(state)
      | Goal(action) =>
        let tasks = GoalHandler.handle(action);
        self.runner->Runner.pushAndRun(tasks);
        Promise.resolved();
      | ViewEvent(event) =>
        let tasks =
          switch (event) {
          | Initialized => []
          | Destroyed => [Task.Terminate]
          };
        self.runner->Runner.pushAndRun(tasks);
        Promise.resolved();
      | Error(error) =>
        let tasks = ErrorHandler.handle(error);
        self.runner->Runner.pushAndRun(tasks);
        Promise.resolved();
      | Debug(message) =>
        let tasks = [Task.displayWarning("Debug", Some(message))];
        self.runner->Runner.pushAndRun(tasks);
        Promise.resolved();
      };
    };
    Runner.setup(self.runner, classifyTask);

    self;
  };

  let dispatchCommand = (self: t, command) => {
    module CommandHandler = Handle__Command.Impl(Editor);
    let tasks = CommandHandler.handle(command);
    self.runner->Runner.pushAndRun(tasks);
  };

  let interrupt = (self, command) => Promise.resolved();

  let destroy = self => {
    Runner.terminate(self.runner);
  };
};