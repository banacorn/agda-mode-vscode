open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);

  type status('payload) =
    // tasks from Agda pending execution
    | Pending('payload)
    // augmented with a promise resolver,
    // which should be triggered after all pending tasks have been executed
    | Closing(unit => unit, 'payload)
    | Free;

  type t = {
    // Theses are the 3 Task queues:
    //    View: for queueing responses from Agda
    //    Agda: for queueing responses from the View
    //    Main: for other tasks
    //
    mutable main: list(Task.t),
    mutable agda: status(list(Task.t)),
    mutable view: status(list(Task.t)),
    // Function for consuming the task queues
    execute: (t, Task.t) => Promise.t(bool),
    // `busy` will be set to `true` if there are Tasks being executed
    // A semaphore to make sure that only one `kickStart` is running at a time
    mutable busy: option(Task.t),
    // invoke the callback (if any) when the task queues are all emptied
    // useful for testing or self destruction
    mutable shouldResolveWhenEmptied: option(unit => unit),
  };

  // Here's the order when retrieving the next task for execution:
  //    View > Agda > Main
  let getNextTask = self => {
    switch (self.view, self.agda, self.main) {
    // still have tasks in the View queue
    | (Pending([x, ...view]), _, _) =>
      self.view = Pending(view);
      Some(x);
    | (Closing(resolve, [x, ...view]), _, _) =>
      self.view = Closing(resolve, view);
      Some(x);
    // stuck on View
    | (Pending([]), _, _) => None
    // trigger the ending promise
    | (Closing(resolve, []), _, _) =>
      self.view = Free;
      resolve();
      None;
    // still have tasks in the Agda queue
    | (Free, Pending([x, ...agda]), _) =>
      self.agda = Pending(agda);
      Some(x);
    | (Free, Closing(resolve, [x, ...agda]), _) =>
      self.agda = Closing(resolve, agda);
      Some(x);
    // stuck on Agda
    | (Free, Pending([]), _) => None
    // trigger the ending promise
    | (Free, Closing(resolve, []), _) =>
      self.agda = Free;
      resolve();
      None;
    // still have tasks in the Main queue
    | (Free, Free, [x, ...main]) =>
      self.main = main;
      Some(x);
    // stuck on Main
    | (Free, Free, []) => None
    };
  };

  let make = execute => {
    main: [],
    agda: Free,
    view: Free,
    execute,
    busy: None,
    shouldResolveWhenEmptied: None,
  };

  // consuming Tasks in the `queues`
  let rec kickStart = self =>
    if (self.busy->Option.isNone) {
      switch (getNextTask(self)) {
      | None =>
        // if there are no more tasks, and .shouldResolveWhenEmptied is set, then resolve it
        switch (self.shouldResolveWhenEmptied) {
        | None => ()
        | Some(resolve) => resolve()
        }
      | Some(task) =>
        // set the semaphore
        switch (task) {
        | AgdaRequest(_) =>
          // self.execute(self, AgdaRequest(_)) would block everything
          self.execute(self, task) // and start executing tasks
          ->Promise.get(keepRunning =>
              if (keepRunning) {
                // and keep running
                kickStart(self);
              }
            )
        | _ =>
          self.busy = Some(task);
          self.execute(self, task) // and start executing tasks
          ->Promise.get(keepRunning => {
              self.busy = None; // flip the semaphore back
              if (keepRunning) {
                // and keep running
                kickStart(self);
              };
            });
        }
      };
    };

  // returns a promise that resolves when all tasks have been executed
  let onEmptied = self =>
    if (self.busy->Option.isSome) {
      let (promise, resolve) = Promise.pending();
      self.shouldResolveWhenEmptied = Some(resolve);
      promise;
    } else {
      Promise.resolved();
    };

  // clear the queue, doesn't wait
  let forceDestroy = self => {
    self.main = [];
    self.agda = Free;
    self.view = Free;
    Promise.resolved();
  };

  // NOTE, currently only DispatchCommand would invoke this
  let addToTheBack = (self, tasks) => {
    self.main = List.concat(self.main, tasks);
    kickStart(self);
  };

  // add tasks to the current **busy** task queue
  let addToTheFront = (self, tasks) => {
    switch (self.view, self.agda) {
    | (Pending(view), _) => self.view = Pending(List.concat(tasks, view))
    | (Closing(resolver, view), _) =>
      self.view = Closing(resolver, List.concat(tasks, view))
    | (Free, Pending(agda)) =>
      self.agda = Pending(List.concat(tasks, agda))
    | (Free, Closing(resolver, agda)) =>
      self.agda = Closing(resolver, List.concat(tasks, agda))
    | (Free, Free) => self.main = List.concat(tasks, self.main)
    };
    kickStart(self);
  };

  let toString = (taskToString, {main, agda, view}) => {
    let main = "Main " ++ Util.Pretty.list(List.map(main, taskToString));
    let agda =
      switch (agda) {
      | Free => ""
      | Pending(agda) =>
        "Agda " ++ Util.Pretty.list(List.map(agda, taskToString))
      | Closing(_, agda) =>
        "Agda# " ++ Util.Pretty.list(List.map(agda, taskToString))
      };
    let view =
      switch (view) {
      | Free => ""
      | Pending(view) =>
        "View " ++ Util.Pretty.list(List.map(view, taskToString))
      | Closing(_, view) =>
        "View# " ++ Util.Pretty.list(List.map(view, taskToString))
      };
    main ++ "\n" ++ agda ++ "\n" ++ view;
  };

  ////////////////////////////////////////////////////////////////////////////////
  // Agda
  ////////////////////////////////////////////////////////////////////////////////

  module Agda = {
    let isOccupied = self =>
      switch (self.agda) {
      | Free => false
      | _ => true
      };

    let close = self =>
      switch (self.agda) {
      | Free => Promise.resolved()
      | Closing(_, _) =>
        Js.log(
          "[ panic ] The Agda task queue has been released by someone else",
        );
        Promise.resolved();
      | Pending(remainingTasks) =>
        let (promise, resolve) = Promise.pending();
        self.agda = Closing(resolve, remainingTasks);
        kickStart(self);
        promise;
      };

    let addToTheBack = (self, tasks) =>
      switch (self.agda) {
      | Free =>
        self.agda = Pending(tasks);
        kickStart(self);
      | Closing(_, _) =>
        Js.log(
          "[ panic ] Cannot add task to the Agda task queue after it's been marked closed",
        )
      | Pending(agda) =>
        self.agda = Pending(List.concat(agda, tasks));
        kickStart(self);
      };
  };

  ////////////////////////////////////////////////////////////////////////////////
  // View
  ////////////////////////////////////////////////////////////////////////////////

  module View = {
    let isOccupied = self =>
      switch (self.view) {
      | Free => false
      | _ => true
      };

    let close = self =>
      switch (self.view) {
      | Free => Promise.resolved()
      | Closing(_, _) =>
        Js.log(
          "[ panic ] The View task queue has been released by someone else",
        );
        Promise.resolved();
      | Pending(remainingTasks) =>
        let (promise, resolve) = Promise.pending();
        self.view = Closing(resolve, remainingTasks);
        kickStart(self);
        promise;
      };

    let addToTheBack = (self, tasks) =>
      switch (self.view) {
      | Free =>
        self.view = Pending(tasks);
        kickStart(self);
      | Closing(_, _) =>
        Js.log(
          "[ panic ] Cannot add task to the View task queue after it's been marked closed",
        )
      | Pending(view) =>
        self.view = Pending(List.concat(view, tasks));
        kickStart(self);
      };
  };
};
