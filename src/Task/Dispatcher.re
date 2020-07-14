open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Decoration = Decoration.Impl(Editor);
  module ErrorHandler = Handle__Error.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);
  module CommandHandler = Handle__Command.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  open! Task;

  let sendAgdaRequest = (runTasks, runTasksLater, state, req) => {
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
      | Ok(Yield(Ok(NonLast(response)))) => {
          Js.log(">>> " ++ Response.toString(response));
          let tasks = ResponseHandler.handle(response);
          runTasks(tasks);
        }
      | Ok(Yield(Ok(Last(priority, response)))) => {
          Js.log(
            ">>> "
            ++ string_of_int(priority)
            ++ " "
            ++ Response.toString(response),
          );
          let tasks = ResponseHandler.handle(response);
          runTasksLater(priority, tasks);
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

  let printLog = false;
  let log =
    if (printLog) {
      Js.log;
    } else {
      _ => ();
    };

  type source =
    | Agda
    | View
    | Command
    | Misc;

  module Runner = {
    type t = {
      mutable queues: MultiQueue.t(Task.t),
      // `busy` will be set to `true` if there are Tasks being executed
      // A semaphore to make sure that only one `kickStart` is running at a time
      mutable busy: bool,
      mutable shouldDestroy: option(unit => unit),
    };

    let make = () => {
      queues: MultiQueue.make(),
      busy: false,
      shouldDestroy: None,
    };

    module Queues = {
      let spawn = (self, target) =>
        self.queues = MultiQueue.spawn(self.queues, target);
      let remove = (self, target) =>
        self.queues = MultiQueue.remove(self.queues, target);
      let addTasks = (self, target, tasks) =>
        self.queues = MultiQueue.addTasks(self.queues, target, tasks);
      let countBySource = (self, target) =>
        MultiQueue.countBySource(self.queues, target);
      let getNextTask = self =>
        MultiQueue.getNextTask(true, self.queues)
        ->Option.map(((task, queue)) => {
            self.queues = queue;
            task;
          });

      let addMiscTasks = (self, tasks) => {
        spawn(self, Misc);
        addTasks(self, Misc, tasks);
        remove(self, Misc);
        Promise.resolved(true);
      };
    };

    let rec executeTask = (self, state: State.t, task) => {
      switch (task) {
      | DispatchCommand(command) =>
        let tasks = CommandHandler.handle(command);
        Queues.addTasks(self, Command, tasks);
        Promise.resolved(true);
      | SendRequest(request) =>
        if (Queues.countBySource(self, Agda) > 0) {
          // there can only be 1 Agda request at a time
          Promise.resolved(
            false,
          );
        } else {
          Queues.spawn(self, Agda);

          let lastTasks = [||];

          sendAgdaRequest(
            tasks => {
              Queues.addTasks(self, Agda, tasks);
              kickStart(self, state);
            },
            (priority, tasks) => {
              Js.Array.push((priority, tasks), lastTasks)->ignore
            },
            state,
            request,
          )
          ->Promise.map(() => {
              // lastTasks
              // ->Array.map(((priority, tasks)) =>
              //     string_of_int(priority)
              //     ++ " "
              //     ++ tasks->List.map(Task.toString)->Util.Pretty.list
              //   )
              // ->Util.Pretty.array
              // ->Js.log;

              let tasks =
                Js.Array.sortInPlaceWith(
                  (x, y) => compare(fst(x), fst(y)),
                  lastTasks,
                )
                ->Array.map(snd)
                ->List.concatMany;

              Queues.addTasks(self, Agda, tasks);
              kickStart(self, state);
            })
          ->Promise.get(() => {Queues.remove(self, Agda)});
          // NOTE: return early before `sendAgdaRequest` resolved
          Promise.resolved(true);
        }
      | SendEventToView(event) =>
        Queues.spawn(self, View);
        state
        ->State.sendEventToView(event)
        ->Promise.map(_ => {
            Queues.remove(self, View);
            true;
          });
      | SendRequestToView(request, callback) =>
        if (Queues.countBySource(self, View) > 0) {
          // there can only be 1 View request at a time
          Promise.resolved(
            false,
          );
        } else {
          Queues.spawn(self, View);
          state
          ->State.sendRequestToView(request)
          ->Promise.map(
              fun
              | None => ()
              | Some(response) => {
                  Queues.addTasks(self, View, callback(response));
                },
            )
          ->Promise.map(() => {
              Queues.remove(self, View);
              true;
            });
        }
      | AddHighlightings(annotations) =>
        annotations->Array.forEach(highlighting => {
          let decorations =
            Decoration.decorateHighlighting(state.editor, highlighting);
          state.decorations = Array.concat(state.decorations, decorations);
        });
        Promise.resolved(true);
      | RemoveAllHighlightings =>
        state.decorations->Array.forEach(Editor.Decoration.destroy);
        state.decorations = [||];
        Promise.resolved(true);
      | RefreshAllHighlightings =>
        //  TextEditorDecorationType
        // Js.log("refresh");
        Promise.resolved(true)
      | WithState(callback) =>
        callback(state);
        Promise.resolved(true);

      | WithStateP(callback) =>
        Queues.spawn(self, Misc);
        callback(state)
        ->Promise.map(Queues.addTasks(self, Misc))
        ->Promise.tap(() => {Queues.remove(self, Misc)})
        ->Promise.map(() => true);

      | SuicideByCop =>
        state->State.emitKillMePlz;
        Promise.resolved(false);

      | Goal(action) =>
        let tasks = GoalHandler.handle(action);
        Queues.addMiscTasks(self, tasks);
      | Error(error) =>
        let tasks = ErrorHandler.handle(error);
        Queues.addMiscTasks(self, tasks);
      | Debug(message) =>
        Js.log("DEBUG " ++ message);
        Promise.resolved(true);
      };
    }
    // consuming Tasks in the `queues`
    and kickStart = (self, state) =>
      if (!self.busy) {
        switch (Queues.getNextTask(self)) {
        | None =>
          // if there are no more tasks, and .shouldDestroy is set, then resolve it
          switch (self.shouldDestroy) {
          | None => ()
          | Some(resolve) => resolve()
          }
        | Some(task) =>
          self.busy = true;
          executeTask(self, state, task) // and start executing tasks
          ->Promise.get(keepRunning => {
              self.busy = false; // flip the semaphore back
              if (keepRunning) {
                // and keep running
                kickStart(self, state);
              };
            });
        };
      };

    // returns a promise that resolves when all tasks have been executed
    let destroy = self =>
      if (self.busy) {
        let (promise, resolve) = Promise.pending();
        self.shouldDestroy = Some(resolve);
        promise;
      } else {
        Promise.resolved();
      };

    // clear the queue, doesn't wait
    let forceDestroy = self => {
      self.queues = MultiQueue.make();
      Promise.resolved();
    };
  };

  type t = {
    blocking: Runner.t,
    critical: Runner.t,
  };

  let make = () => {blocking: Runner.make(), critical: Runner.make()};

  let dispatchCommand = (self, state: State.t, command) => {
    log(
      "\n\n"
      ++ MultiQueue.toString(Task.toString, self.critical.queues)
      ++ "\n----------------------------\n"
      ++ MultiQueue.toString(Task.toString, self.blocking.queues),
    );

    switch (command) {
    | Command.NextGoal
    | PreviousGoal
    | InputMethod(_)
    | EventFromView(_)
    | Escape =>
      Runner.Queues.addTasks(
        self.critical,
        Command,
        [DispatchCommand(command)],
      );
      Runner.kickStart(self.critical, state);
    | _ =>
      Runner.Queues.addTasks(
        self.blocking,
        Command,
        [DispatchCommand(command)],
      );
      Runner.kickStart(self.blocking, state);
    };
  };

  // wait until all tasks have finished executing
  let destroy = self => {
    self.critical
    ->Runner.destroy
    ->Promise.flatMap(() => self.blocking->Runner.destroy);
  };

  // destroy everything in a rather violent way
  let forceDestroy = self => {
    self.critical
    ->Runner.forceDestroy
    ->Promise.flatMap(() => self.blocking->Runner.forceDestroy);
  };
};
