open Belt;

type t('task) = {
  // Theses are the 3 Task queues:
  //    View: for queueing responses from Agda
  //    Agda: for queueing responses from the View
  //    Main: for other tasks
  //
  // Take the Agda queue for example, when it's:
  //    Some([Task0, Task1, ...])   : there are tasks from Agda pending execution
  //    Some([])                    : still waiting for Agda to respond or terminate
  //    None                        : not waiting for Agda
  //
  mutable main: list('task),
  mutable agda: option(list('task)),
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
  | (Some([x, ...view]), _, _) =>
    self.view = Some(view);
    Some(x);
  | (Some([]), _, _) => None // stuck on View
  | (None, Some([x, ...agda]), _) =>
    self.agda = Some(agda);
    Some(x);
  | (None, Some([]), _) => None // stuck on Agda
  | (None, None, [x, ...main]) =>
    self.main = main;
    Some(x);
  | (None, None, []) => None
  };
};

let make = execute => {
  main: [],
  agda: None,
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
  self.agda = None;
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
    | None => ""
    | Some(agda) =>
      "Agda " ++ Util.Pretty.list(List.map(agda, taskToString))
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

let agdaIsOccupied = self => self.agda->Option.isSome;

let acquireAgda = self =>
  switch (self.agda) {
  | None => self.agda = Some([])
  | Some(_) =>
    Js.log("[ panic ] The Agda task queue has already been acquired")
  };

let releaseAgda = self =>
  switch (self.agda) {
  | None => ()
  | Some(remainingTasks) =>
    // concat the remaining tasks to the main task queue
    self.main = List.concat(remainingTasks, self.main);
    self.agda = None;
  };

let addTasksToAgda = (self, tasks) =>
  switch (self.agda) {
  | None =>
    Js.log(
      "[ panic ] Cannot add task to the Agda task queue before acquiring it",
    )
  | Some(agda) => self.agda = Some(List.concat(agda, tasks))
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
