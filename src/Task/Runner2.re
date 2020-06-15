open Belt;
type status =
  | Busy
  | Idle;

type packet('a) =
  | Wrapped('a)
  | Pending(unit => unit);

type t('a) = {
  mutable externalQueue: array(packet('a)),
  mutable internalQueue: array(packet('a)),
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
    externalQueue: [||],
    internalQueue: [||],
    status: Idle,
    execute: None,
    terminationPromise: promise,
    terminate: resolve,
    shouldTerminate: false,
  };
};

let setup = (self, execute) => self.execute = Some(execute);

let rec run = (self: t('a)): unit => {
  let getNext = self =>
    switch (Js.Array.shift(self.internalQueue)) {
    | None => Js.Array.shift(self.externalQueue)
    | Some(x) => Some(x)
    };

  switch (self.status) {
  // only one `run` should be running at a time
  | Busy => ()
  | Idle =>
    switch (getNext(self)) {
    | None => ()
    | Some(packet) =>
      self.status = Busy;
      switch (self.execute) {
      | None =>
        self.status = Idle;
        run(self);
      | Some(execute) =>
        switch (packet) {
        | Wrapped(task) => execute(task)->Promise.get(() => {run(self)})
        | Pending(resolve) =>
          resolve();
          run(self);
        }
      };
    }
  };
  // see if the runner is responsible of invoking `terminate`
  // after finished executing all tasks in the queue
  if (self.shouldTerminate) {
    self.terminate();
  };
};

let pushToBackAndRun = (self: t('a), xs: list('a)): Promise.t(unit) => {
  // wrap them up
  let xs = List.map(xs, x => Wrapped(x))->List.toArray;
  // add a pending resolve to the back
  let (promise, resolve) = Promise.pending();
  Js.Array.push(Pending(resolve), xs)->ignore;
  // concat to the back of the queue
  self.externalQueue = Js.Array.concat(self.externalQueue, xs);
  // kick start the runner, and wait until the promise resolves
  run(self);
  promise;
};

let pushToFrontAndRun = (self: t('a), xs: list('a)): Promise.t(unit) => {
  // wrap them up
  let xs = List.map(xs, x => Wrapped(x))->List.toArray;
  // add a pending resolve to the back
  let (promise, resolve) = Promise.pending();
  Js.Array.push(Pending(resolve), xs)->ignore;
  // concat to the front of the queue
  self.internalQueue = Js.Array.concat(xs, self.internalQueue);
  // kick start the runner, and wait until the promise resolves
  run(self);
  promise;
};

// If the runner is currently Idle,
// then resolve the termination promise immediately
// else set `shouldTerminate` and wait for the runner to resolve the termination promise
let terminate = self =>
  switch (self.status) {
  | Idle => self.terminate()
  | Busy => self.shouldTerminate = true
  };