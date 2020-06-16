open Belt;

type status =
  | Busy
  | Idle;

type t('a) = {
  mutable queue: list('a),
  mutable status,
  // the work horse, stops when the promise resolves to false
  mutable execute: option('a => Promise.t(bool)),
  // invoke `terminate` to resolve `terminationPromise`
  terminationPromise: Promise.t(unit),
  terminate: unit => unit,
  // this flag is set to True when the runner should be terminated despite that it's still running
  // transfer the responsibility of invoking `terminate` to the runner
  mutable shouldTerminate: bool,
};

let empty = self => {
  let queue = self.queue;
  self.queue = [];
  queue;
};

let make = () => {
  let (promise, resolve) = Promise.pending();
  {
    queue: [],
    status: Idle,
    execute: None,
    terminationPromise: promise,
    terminate: resolve,
    shouldTerminate: false,
  };
};

// late-binding `Runner.execute`
// `Runner.execute` may refer to the `Runner` itself,
// making it impossible to define `Runner.execute` whilst defining `Runner.make`
let setup = (self, execute) => self.execute = Some(execute);

let rec run = (self: t('a)): unit => {
  switch (self.status) {
  // only one `run` should be running at a time
  | Busy => ()
  | Idle =>
    switch (self.queue) {
    | [] => ()
    | [task, ...queue] =>
      self.status = Busy;
      self.queue = queue;
      switch (self.execute) {
      | None =>
        self.status = Idle;
        run(self);
      | Some(execute) =>
        execute(task)
        ->Promise.get(keepRunning => {
            self.status = Idle;
            if (keepRunning) {
              run(self);
            } else {
              Js.log("RUNNER STOPED");
            };
          })
      };
    }
  };
  // see if the runner is responsible of invoking `terminate`
  // after finished executing all tasks in the queue
  if (self.shouldTerminate) {
    self.terminate();
  };
};

let pushAndRun = (self: t('a), xs: list('a)): unit => {
  // concat to the back of the queue
  self.queue = List.concat(self.queue, xs);
  // kick start the runner
  run(self);
};

// If the runner is currently Idle,
// then resolve the termination promise immediately
// else set `shouldTerminate` and wait for the runner to resolve the termination promise
let terminate = self =>
  switch (self.status) {
  | Idle => self.terminate()
  | Busy => self.shouldTerminate = true
  };