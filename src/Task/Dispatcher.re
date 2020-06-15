open Belt;

module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Handle__Error.Impl(Editor);
  // module ViewHandler = Handle__View.Impl(Editor);

  module GoalHandler = Handle__Goal.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  // module State = State.Impl(Editor);
  // module Request = Request.Impl(Editor);
  // type t = Runner.t(Command.t);
  open! Task;

  type t = {
    agdaRequestRunner: Runner2.t(Request.t),
    // mutable agdaRequestStatus: status(Request.t),
    viewRequestRunner: Runner2.t(View.Request.t),
    // mutable viewRequestStatus: status(View.Request.t),
    generalTaskRunner: Runner2.t(Task.t),
  };

  let dispatchCommand = (self, command) => {
    module CommandHandler = Handle__Command.Impl(Editor);
    let tasks = CommandHandler.handle(command);
    self.generalTaskRunner
    ->Runner2.pushAndRun(tasks)
    ->Promise.tap(() => {
        Js.log(Command.toString(command) ++ " completed");
        Js.log(self);
      });
  };

  let sendAgdaRequest = (runTasks, state, req) => {
    // this promise get resolved after the request to Agda is completed
    let (promise, resolve) = Promise.pending();
    let handle = ref(None);
    let handler: result(Connection.response, Connection.Error.t) => unit =
      fun
      | Error(error) => {
          let tasks = ErrorHandler.handle(Error.Connection(error));
          runTasks(tasks)->Promise.get(resolve);
        }
      | Ok(Parser.Incr.Event.Yield(Error(error))) => {
          let tasks = ErrorHandler.handle(Error.Parser(error));
          runTasks(tasks)->Promise.get(resolve);
        }
      | Ok(Yield(Ok(response))) => {
          Js.log(">>> " ++ Response.toString(response));
          let tasks = ResponseHandler.handle(response);
          runTasks(tasks)->ignore;
        }
      | Ok(Stop) => resolve();

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
            runTasks(tasks)->Promise.get(resolve);
            promise;
          },
      )
    ->Promise.tap(() => (handle^)->Option.forEach(f => f()));
  };

  let make = state => {
    let self = {
      agdaRequestRunner: Runner2.make(),
      // agdaRequestStatus: Available,
      viewRequestRunner: Runner2.make(),
      // viewRequestStatus: Available,
      generalTaskRunner: Runner2.make(),
    };

    self.generalTaskRunner
    ->Runner2.setup(task => {
        Js.log("general " ++ Task.toString(task));
        switch (task) {
        | SendRequest(req) =>
          self.agdaRequestRunner
          ->Runner2.pushAndRun([req])
          ->Promise.map(() => [])
        // ->Runner2.pushAndRun([req])
        // let runTasks = tasks => {
        //   self.generalTaskRunner->Runner2.pushInternal(tasks);
        //   Promise.resolved();
        // };
        // sendAgdaRequest(runTasks, state, req)->Promise.map(() => []);
        | ViewReq(req, callback) =>
          state
          ->State.sendRequestToView(req)
          ->Promise.map(response => {
              let tasks = callback(response);
              Js.log(
                "View Response: "
                ++ Util.Pretty.array(
                     List.toArray(List.map(tasks, Task.toString)),
                   ),
              );
              self.generalTaskRunner->Runner2.pushInternal(tasks);
              [];
            })
        | Terminate => State.destroy(state)->Promise.map(() => [])
        | WithState(callback) => callback(state)
        | Goal(action) => GoalHandler.handle(action)->Promise.resolved
        | ViewEvent(event) =>
          switch (event) {
          | Initialized => Promise.resolved([])
          | Destroyed => Promise.resolved([Task.Terminate])
          }
        | Error(error) => ErrorHandler.handle(error)->Promise.resolved
        | Debug(message) =>
          Promise.resolved([Task.displayWarning("Debug", Some(message))])
        };
      });

    self.agdaRequestRunner
    ->Runner2.setup(req => {
        Js.log("agdaRequestRunner");
        sendAgdaRequest(
          tasks => {
            Js.log("push");
            self.generalTaskRunner->Runner2.pushInternal(tasks);
            Promise.resolved();
          },
          state,
          req,
        )
        ->Promise.map(() => []);
      });

    self.viewRequestRunner
    ->Runner2.setup(req => {
        Js.log("viewRequestRunner");
        Js.log(req);
        Promise.resolved([]);
      });

    self;
  };

  let interrupt = (self, command) => {
    module CommandHandler = Handle__Command.Impl(Editor);
    let tasks = CommandHandler.handle(command);
    self.generalTaskRunner->Runner2.pushAndRun(tasks);
  };

  let destroy = self => {
    self.agdaRequestRunner->Runner2.terminate;
    self.viewRequestRunner->Runner2.terminate;
    self.generalTaskRunner->Runner2.terminate;
  };
};