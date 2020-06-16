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
    let queue = Runner.empty(self.runner);
    self.blockedQueues = [(resource, queue), ...self.blockedQueues];
  };

  let addTasksBy = (self, resource, tasks) => {
    // find the queue that the resource uses (before the queue it blocks)
    // and concat tasks after that queue
    let rec go =
      fun
      | [] => (false, [])
      | [(kind0, queue0)] =>
        if (kind0 == resource) {
          (true, [(kind0, queue0)]);
        } else {
          (false, [(kind0, queue0)]);
        }
      | [(kind0, queue0), ...queues] =>
        if (kind0 == resource) {
          (true, [(kind0, queue0), ...queues]);
        } else {
          let (shouldConcat, queues) = go(queues);
          if (shouldConcat) {
            (false, [(kind0, List.concat(queue0, tasks)), ...queues]);
          } else {
            (false, [(kind0, queue0), ...queues]);
          };
        };

    let (shouldConcatToRunner, queues) = go(self.blockedQueues);

    if (shouldConcatToRunner) {
      Runner.pushAndRun(self.runner, tasks);
    } else {
      self.blockedQueues = queues;
    };
  };

  let addTasks = (self, tasks) => {
    let rec go =
      fun
      | [] => (true, [])
      | [(kind, queue)] => (false, [(kind, List.concat(queue, tasks))])
      | [(kind, queue), ...queues] => {
          let (_, queues) = go(queues);
          (false, [(kind, queue), ...queues]);
        };

    let (shouldConcatToRunner, queues) = go(self.blockedQueues);

    if (shouldConcatToRunner) {
      Runner.pushAndRun(self.runner, tasks);
    } else {
      self.blockedQueues = queues;
    };
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
    | None => self.runner->Runner.run
    | Some(queue) => self.runner->Runner.pushAndRun(queue)
    };
  };
  let logStatus = (self, task) => {
    switch (task) {
    | Some(task) =>
      Js.log(
        "\nTask: "
        ++ Task.toString(task)
        ++ "\nRunner: "
        ++ Util.Pretty.list(List.map(self.runner.queue, Task.toString))
        ++ "\n===============================",
      )
    | None =>
      Js.log(
        "\nRunner: "
        ++ Util.Pretty.list(List.map(self.runner.queue, Task.toString))
        ++ "\n===============================",
      )
    };
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
      logStatus(self, Some(task));
      switch (task) {
      | ViewReq(request, callback) =>
        if (blockedBy(self, View)) {
          Js.log("BLOCKED BY VIEW");

          Promise.resolved(false);
        } else {
          acquire(self, View);
          state
          ->State.sendRequestToView(request)
          ->Promise.map(response => {
              addTasksBy(self, View, callback(response));
              release(self, View);
              true;
            });
        }
      | SendRequest(request) =>
        if (blockedBy(self, Agda)) {
          Promise.resolved(false);
        } else {
          acquire(self, Agda);
          // issue request
          sendAgdaRequest(
            tasks => {
              logStatus(self, Some(task));
              addTasksBy(self, Agda, tasks);
            },
            state,
            request,
          )
          ->Promise.get(() => {release(self, Agda)});
          // NOTE: early return before `sendAgdaRequest` resolved
          Promise.resolved(true);
        }
      | WithState(callback) =>
        callback(state)
        ->Promise.map(tasks => {
            self.runner->Runner.pushAndRun(tasks);
            true;
          })

      | Terminate => State.destroy(state)->Promise.map(() => false)
      | Goal(action) =>
        let tasks = GoalHandler.handle(action);
        self.runner->Runner.pushAndRun(tasks);
        Promise.resolved(true);
      | ViewEvent(event) =>
        let tasks =
          switch (event) {
          | Initialized => []
          | Destroyed => [Task.Terminate]
          };
        self.runner->Runner.pushAndRun(tasks);
        Promise.resolved(true);
      | Error(error) =>
        let tasks = ErrorHandler.handle(error);
        self.runner->Runner.pushAndRun(tasks);
        Promise.resolved(true);
      | Debug(message) =>
        let tasks = [Task.displayWarning("Debug", Some(message))];
        self.runner->Runner.pushAndRun(tasks);
        Promise.resolved(true);
      };
    };
    Runner.setup(self.runner, classifyTask);

    self;
  };

  let dispatchCommand = (self: t, state: State.t, command) => {
    logStatus(self, None);
    open Command;
    module CommandHandler = Handle__Command.Impl(Editor);
    switch (command) {
    // HACKY interrupt
    | Escape =>
      if (state.inputMethod.activated) {
        let tasks = CommandHandler.handle(InputSymbol(Deactivate));
        addTasks(self, tasks);
      } else {
        state
        ->State.sendRequestToView(View.Request.InterruptQuery)
        ->Promise.get(_ => ());
      }
    | _ =>
      let tasks = CommandHandler.handle(command);
      addTasks(self, tasks);
    };
  };

  let destroy = self => {
    Runner.terminate(self.runner);
  };
};