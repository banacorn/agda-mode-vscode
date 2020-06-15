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
    | BlockedByAgda(array(Task.t))
    | BlockedByView(array(Task.t));

  type t = {
    runner: Runner3.t(Task.t),
    mutable blockedQueues: array(blockedQueue),
  };

  // // let dispense = (self: t): Promise.t(Task.t) => {};
  // let rec run = (self, state) =>
  //   fun
  //   | [] => Promise.resolved()
  //   | [task, ...tasks] =>
  //     switch (self.general) {
  //     | Occupied => Promise.resolved()
  //     | Available =>
  //       switch (task) {
  //       | NonBlocking(task) =>
  //         Js.log("NonBlocking Task: " ++ Task.toString(task));
  //         switch (task) {
  //         | SendRequest(request) =>
  //           switch (self.agda) {
  //           | Occupied =>
  //             Js.log("Status: agda OCCUPIED");
  //             Promise.resolved(); // ???
  //           | Available =>
  //             self.agda = Occupied;
  //             let queue = ref([||]);
  //             Dispatcher.sendAgdaRequest(
  //               tasks' => {run(self, state, List.concat(tasks', tasks))},
  //               state,
  //               request,
  //             )
  //             ->Promise.tap(() => {self.agda = Available});
  //           }
  //         | WithState(callback) =>
  //           callback(state)
  //           ->Promise.flatMap(tasks' => {
  //               let tasks' = tasks'->List.map(task => NonBlocking(task));
  //               run(self, state, List.concat(tasks', tasks));
  //             })
  //         | otherTask => run(self, state, tasks)
  //         };
  //       | Blocking(tasks) =>
  //         Js.log(
  //           "Blocking Task: "
  //           ++ Util.Pretty.array(Array.map(tasks, Task.toString)),
  //         );
  //         Promise.resolved();
  //       };

  //       switch (task) {
  //       // | SendRequest(request) =>
  //       //   Js.log("Task: " ++ Task.toString(task));
  //       //   switch (self.agda) {
  //       //   | Occupied =>
  //       //     Js.log("Status: agda OCCUPIED");
  //       //     Promise.resolved(); // ???
  //       //   | Available =>
  //       //     self.agda = Occupied;
  //       //     Dispatcher.sendAgdaRequest(
  //       //       tasks' => {
  //       //         Js.log(
  //       //           "Derived tasks: "
  //       //           ++ Util.Pretty.array(
  //       //                List.toArray(List.map(tasks', Task.toString)),
  //       //              ),
  //       //         );
  //       //         Js.log(
  //       //           "Other tasks: "
  //       //           ++ Util.Pretty.array(
  //       //                List.toArray(List.map(tasks, Task.toString)),
  //       //              ),
  //       //         );
  //       //         run(self, state, List.concat(tasks', tasks));
  //       //       },
  //       //       state,
  //       //       request,
  //       //     )
  //       //     ->Promise.tap(() => {self.agda = Available});
  //       //   };
  //       // | WithState(callback) =>
  //       //   callback(state)
  //       //   ->Promise.flatMap(tasks' => {
  //       //       run(self, state, List.concat(tasks', tasks))
  //       //     })
  //       | otherTask => run(self, state, tasks)
  //       };
  //     };

  let dispatchCommand = (self, state, command) => {
    module CommandHandler = Handle__Command.Impl(Editor);
    let tasks = CommandHandler.handle(command);
    self.runner->Runner3.pushAndRun(tasks);
  };
  let make = state => {
    let blockedQueues = ref([||]);

    let agdaIsOccupied = queues =>
      queues->Array.some(
        fun
        | BlockedByAgda(_) => true
        | BlockedByView(_) => false,
      );

    let runner = Runner3.make();
    let classifyTask = task => {
      Js.log("Task: " ++ Task.toString(task));
      switch (task) {
      | SendRequest(request) =>
        if (agdaIsOccupied(blockedQueues^)) {
          Js.log("Agda blocked");
          Promise.resolved();
        } else {
          Js.log("Agda not blocked");
          // empty the current Runner
          let queue = Runner3.empty(runner);
          Js.Array.unshift(BlockedByAgda(queue), blockedQueues^)->ignore;
          Promise.resolved();
        }
      // | WithState(callback) =>
      //   callback(state)
      //   ->Promise.flatMap(tasks => {runner->Runner3.pushAndRun(tasks)})
      //   ->Promise.map(() => [])
      | others => Promise.resolved()
      };
    };

    Runner3.setup(runner, classifyTask);
    {runner, blockedQueues: [||]};
  };

  let interrupt = (self, command) => Promise.resolved();

  let destroy = _ => ();
};