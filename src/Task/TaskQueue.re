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
  main: list('task),
  agda: option(list('task)),
  view: option(list('task)),
};

// Here's the order when retrieving the next task for execution:
//    View > Agda > Main
let getNextTask = self => {
  switch (self.view, self.agda, self.main) {
  | (Some([x, ...view]), agda, main) => (
      Some(x),
      {view: Some(view), agda, main},
    )
  | (Some([]), _, _) => (None, self) // stuck on View
  | (None, Some([x, ...agda]), main) => (
      Some(x),
      {view: None, agda: Some(agda), main},
    )
  | (None, Some([]), _) => (None, self) // stuck on Agda
  | (None, None, [x, ...main]) => (
      Some(x),
      {view: None, agda: None, main},
    )
  | (None, None, []) => (None, self)
  };
};

let make = () => {main: [], agda: None, view: None};

let addTasksToMain = ({main, agda, view}, tasks) => {
  main: List.concat(main, tasks),
  agda,
  view,
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

let acquireAgda = ({main, agda, view}) =>
  switch (agda) {
  | None => {main, agda: Some([]), view}
  | Some(_) =>
    Js.log("[ panic ] The Agda task queue has already been acquired");
    {main, agda, view};
  };

let releaseAgda = ({main, agda, view}) =>
  switch (agda) {
  | None => {main, agda, view}
  | Some(remainingTasks) => {
      // concat the remaining tasks to the main task queue
      main: List.concat(remainingTasks, main),
      agda: None,
      view,
    }
  };

let addTasksToAgda = ({main, agda, view}, tasks) =>
  switch (agda) {
  | None =>
    Js.log(
      "[ panic ] Cannot add task to the Agda task queue before acquiring it",
    );
    {main, agda, view};
  | Some(agda) => {main, agda: Some(List.concat(agda, tasks)), view}
  };

////////////////////////////////////////////////////////////////////////////////
// View
////////////////////////////////////////////////////////////////////////////////

let viewIsOccupied = self => self.view->Option.isSome;

let acquireView = ({main, agda, view}) =>
  switch (view) {
  | None => {main, agda, view: Some([])}
  | Some(_) =>
    Js.log("[ panic ] The View task queue has already been acquired");
    {main, agda: None, view};
  };

let releaseView = ({main, agda, view}) =>
  switch (view) {
  | None => {main, agda, view}
  | Some(remainingTasks) => {
      // concat the remaining tasks to the main task queue
      main: List.concat(remainingTasks, main),
      agda,
      view: None,
    }
  };

let addTasksToView = ({main, agda, view}, tasks) =>
  switch (view) {
  | None =>
    Js.log(
      "[ panic ] Cannot add task to the View task queue before acquiring it",
    );
    {main, agda, view};
  | Some(view) => {main, agda, view: Some(List.concat(view, tasks))}
  };
