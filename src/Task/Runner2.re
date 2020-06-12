open Belt;
type status =
  | Busy
  | Idle;

type packet('a) =
  | Wrapped('a)
  | Pending(unit => unit);

type t('a) = {
  mutable queue: array(packet('a)),
  mutable status,
  // the work horse
  mutable execute: option('a => Promise.t(unit)),
  // invoke `terminate` to resolve `terminationPromise`
  // terminationPromise: Promise.t(unit),
  // terminate: unit => unit,
  // this flag is set to True when the runner should be terminated despite that it's still running
  // transfer the responsibility of invoking `terminate` to the runner
  // mutable shouldTerminate: bool,
};

let make = () => {
  {
    queue: [||],
    status: Idle,
    execute: None,
    // terminationPromise: promise,
    // terminate: resolve,
    // shouldTerminate: false,
  };
};

let setup = (self, execute) => self.execute = Some(execute);

let rec run = (self: t('a)): unit =>
  switch (self.status) {
  // only one `run` should be running at a time
  | Busy => ()
  | Idle =>
    let nextPackets = Js.Array.shift(self.queue);
    switch (nextPackets) {
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
    };
  // see if the runner is responsible of invoking `terminate`
  // after finished executing all tasks in the queue
  // ->Promise.tap(() =>
  //     if (self.shouldTerminate) {
  //       self.terminate();
  //     }
  //   );
  };

// let push = (self, x: 'a) => {
//   // push a new task to the queue
//   Js.Array.push(x, self.queue)->ignore;
//   // kick start the runner
//   run(self);
// };

let pushToBackAndRun = (self: t('a), xs: list('a)): Promise.t(unit) => {
  // wrap them up
  let xs = List.map(xs, x => Wrapped(x))->List.toArray;
  // add a pending resolve to the back
  let (promise, resolve) = Promise.pending();
  Js.Array.push(Pending(resolve), xs)->ignore;
  // concat to the back of the queue
  self.queue = Js.Array.concat(self.queue, xs);
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
  self.queue = Js.Array.concat(xs, self.queue);
  // kick start the runner, and wait until the promise resolves
  run(self);
  promise;
};