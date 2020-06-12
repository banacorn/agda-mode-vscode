type status =
  | Busy
  | Idle;

type t('a) = {
  mutable queue: array('a),
  mutable status,
  // the work horse
  mutable execute: option('a => Promise.t(unit)),
  // invoke `terminate` to resolve `terminationPromise`
  terminationPromise: Promise.t(unit),
  terminate: unit => unit,
  // this flag is set to True when the runner should be terminated despite that it's still running
  // transfer the responsibility of invoking `terminate` to the runner
  mutable shouldTerminate: bool,
};

let make = () => {
  let (promise, resolve) = Promise.pending();
  {
    queue: [||],
    status: Idle,
    execute: None,
    terminationPromise: promise,
    terminate: resolve,
    shouldTerminate: false,
  };
};

let setup = (self, execute) => self.execute = Some(execute);

let rec run = (self: t('a)): Promise.t(unit) =>
  switch (self.status) {
  // only one `run` should be running at a time
  | Busy => Promise.resolved()
  | Idle =>
    let nextTasks = Js.Array.shift(self.queue);
    (
      switch (nextTasks) {
      | None => Promise.resolved()
      | Some(task) =>
        self.status = Busy;
        switch (self.execute) {
        | None =>
          self.status = Idle;
          run(self);
        | Some(execute) =>
          execute(task)
          ->Promise.tap(_ => {self.status = Idle})
          ->Promise.flatMap(() => {run(self)})
        };
      }
    )
    // see if the runner is responsible of invoking `terminate`
    // after finished executing all tasks in the queue
    ->Promise.tap(() =>
        if (self.shouldTerminate) {
          self.terminate();
        }
      );
  };

let push = (self, x: 'a) => {
  // push a new task to the queue
  Js.Array.push(x, self.queue)->ignore;
  // kick start the runner
  run(self);
};

let pushMany = (self: t('a), xs: array('a)): Promise.t(unit) => {
  // concat tasks to the back of the queue
  self.queue = Js.Array.concat(self.queue, xs);
  // kick start the runner
  run(self);
};

let pushManyToFront = (self: t('a), xs: array('a)): Promise.t(unit) => {
  // concat tasks to the from of the queue
  self.queue = Js.Array.concat(xs, self.queue);
  // kick start the runner
  run(self);
};

// NOTE: hacky ...
let interrupt = (self, task: 'a) => {
  switch (self.execute) {
  | None => Promise.resolved()
  | Some(execute) => execute(task)
  };
};

// If the runner is currently Idle,
// then resolve the termination promise immediately
// else set `shouldTerminate` and wait for the runner to resolve the termination promise
let terminate = self =>
  switch (self.status) {
  | Idle => self.terminate()
  | Busy => self.shouldTerminate = true
  };