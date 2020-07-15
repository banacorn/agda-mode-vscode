open Belt;

type status('payload) =
  // tasks from Agda pending execution
  | Pending('payload)
  // augmented with a promise resolver, should be triggered when all pending tasks have been executed
  | Ending(unit => unit, 'payload)
  | Free;

type t('task) = {
  // Theses are the 3 Task queues:
  //    View: for queueing responses from Agda
  //    Agda: for queueing responses from the View
  //    Main: for other tasks
  //
  mutable main: list('task),
  mutable agda: status(list('task)),
  mutable view: option(list('task)),
  // Function for consuming the task queues
  execute: (t('task), t('task) => unit, 'task) => Promise.t(bool),
  // `busy` will be set to `true` if there are Tasks being executed
  // A semaphore to make sure that only one `kickStart` is running at a time
  mutable busy: bool,
  mutable shouldDestroy: option(unit => unit),
};

// Here's the order when retrieving the next task for execution:
//    View > Agda > Main
let getNextTask = self => {
  switch (self.view, self.agda, self.main) {
  // still have tasks in the View queue
  | (Some([x, ...view]), _, _) =>
    self.view = Some(view);
    Some(x);
  // stuck on View
  | (Some([]), _, _) => None
  // still have tasks in the Agda queue
  | (None, Pending([x, ...agda]), _) =>
    self.agda = Pending(agda);
    Some(x);
  | (None, Ending(resolve, [x, ...agda]), _) =>
    self.agda = Ending(resolve, agda);
    Some(x);
  // stuck on Agda
  | (None, Pending([]), _) => None
  // trigger the ending promise
  | (None, Ending(resolve, []), _) =>
    self.agda = Free;
    resolve();
    None;
  // still have tasks in the Main queue
  | (None, Free, [x, ...main]) =>
    self.main = main;
    Some(x);
  // stuck on Main
  | (None, Free, []) => None
  };
};

let make = execute => {
  main: [],
  agda: Free,
  view: None,
  execute,
  busy: false,
  shouldDestroy: None,
};

// consuming Tasks in the `queues`
let rec kickStart = self =>
  if (!self.busy) {
    switch (getNextTask(self)) {
    | None =>
      // if there are no more tasks, and .shouldDestroy is set, then resolve it
      switch (self.shouldDestroy) {
      | None => ()
      | Some(resolve) => resolve()
      }
    | Some(task) =>
      self.busy = true;
      self.execute(self, kickStart, task) // and start executing tasks
      ->Promise.get(keepRunning => {
          self.busy = false; // flip the semaphore back
          if (keepRunning) {
            // and keep running
            kickStart(self);
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
  self.main = [];
  self.agda = Free;
  self.view = None;
  Promise.resolved();
};

let addTasksToBack = (self, tasks) => {
  self.main = List.concat(self.main, tasks);
};

let addTasksToFront = (self, tasks) => {
  self.main = List.concat(tasks, self.main);
};

let toString = (taskToString, {main, agda, view}) => {
  let main = "Main " ++ Util.Pretty.list(List.map(main, taskToString));
  let agda =
    switch (agda) {
    | Free => ""
    | Pending(agda) =>
      "Agda " ++ Util.Pretty.list(List.map(agda, taskToString))
    | Ending(_, agda) =>
      "Agda# " ++ Util.Pretty.list(List.map(agda, taskToString))
    };
  let view =
    switch (view) {
    | None => ""
    | Some(view) =>
      "View " ++ Util.Pretty.list(List.map(view, taskToString))
    };
  main ++ "\n" ++ agda ++ "\n" ++ view;
};

////////////////////////////////////////////////////////////////////////////////
// Agda
////////////////////////////////////////////////////////////////////////////////

let agdaIsOccupied = self =>
  switch (self.agda) {
  | Free => false
  | _ => true
  };

let acquireAgda = self =>
  switch (self.agda) {
  | Free => self.agda = Pending([])
  | _ => Js.log("[ panic ] The Agda task queue has already been acquired")
  };

let releaseAgda = self =>
  switch (self.agda) {
  | Free => Promise.resolved()
  | Ending(_, _) =>
    Js.log("[ panic ] The Agda task queue has been released by someone else");
    Promise.resolved();
  | Pending(remainingTasks) =>
    let (promise, resolve) = Promise.pending();
    self.agda = Ending(resolve, remainingTasks);
    kickStart(self);
    promise;
  };

let addTasksToAgda = (self, tasks) =>
  switch (self.agda) {
  | Free =>
    Js.log(
      "[ panic ] Cannot add task to the Agda task queue before acquiring it",
    )
  | Ending(_, _) =>
    Js.log(
      "[ panic ] Cannot add task to the Agda task queue when it's ending",
    )
  | Pending(agda) => self.agda = Pending(List.concat(agda, tasks))
  };

////////////////////////////////////////////////////////////////////////////////
// View
////////////////////////////////////////////////////////////////////////////////

let viewIsOccupied = self => self.view->Option.isSome;

let acquireView = self =>
  switch (self.view) {
  | None => self.view = Some([])
  | Some(_) =>
    Js.log("[ panic ] The View task queue has already been acquired")
  };

let releaseView = self =>
  switch (self.view) {
  | None => ()
  | Some(remainingTasks) =>
    // concat the remaining tasks to the main task queue
    self.main = List.concat(remainingTasks, self.main);
    self.view = None;
  };

let addTasksToView = (self, tasks) =>
  switch (self.view) {
  | None =>
    Js.log(
      "[ panic ] Cannot add task to the View task queue before acquiring it",
    )
  | Some(view) => self.view = Some(List.concat(view, tasks))
  };
