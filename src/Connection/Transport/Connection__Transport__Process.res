// module for communicating with a process

module Event = {
  type exitCode = int
  type signal = string
  type path = string
  type args = array<string>
  type t =
    | OnDestroyed // on `disconnect` (destroyed by the user)
    | OnError(string) // on `error`
    | OnExit(exitCode) // on `exit` or `close`

  let toString = x =>
    switch x {
    | OnDestroyed => "Process destroyed"
    | OnError(error) => error
    | OnExit(code) => "Process exited with code " ++ string_of_int(code)
    }
}

// examine if the event indicates that the command is not found
let errorMessageIndicatesNotFound = msg =>
  RegExp.test(%re("/command not found/"), msg) ||
  RegExp.test(%re("/No such file or directory/"), msg) ||
  RegExp.test(%re("/not found/"), msg) ||
  RegExp.test(%re("/The system cannot find the path specified/"), msg) ||
  String.endsWith(msg, "ENOENT")

module type Module = {
  type t
  // lifetime: same as the child process
  let make: (string, array<string>) => t
  let destroy: t => promise<unit>
  // messaging
  let send: (t, string) => bool
  // events
  type output =
    | Stdout(string)
    | Stderr(string)
    | Event(Event.t)
  let onOutput: (t, output => unit) => unit => unit
}
module Module: Module = {
  type output =
    | Stdout(string)
    | Stderr(string)
    | Event(Event.t)

  // internal status
  type status =
    | Created(NodeJs.ChildProcess.t)
    | Destroying(promise<unit>)
    | Destroyed

  type t = {
    chan: Chan.t<output>,
    mutable status: status,
  }

  let make = (path, args) => {
    let chan = Chan.make()
    let stderr = ref("")
    // spawn the child process
    let process = NodeJs.ChildProcess.spawnWith("\"" ++ path ++ "\"", args, %raw(`{shell : true}`))

    // on `data` from `stdout`
    process
    ->NodeJs.ChildProcess.stdout
    ->Option.forEach(stream =>
      stream
      ->NodeJs.Stream.onData(chunk => {
        chan->Chan.emit(Stdout(NodeJs.Buffer.toString(chunk)))
      })
      ->ignore
    )

    // on `data` from `stderr`
    process
    ->NodeJs.ChildProcess.stderr
    ->Option.forEach(stream =>
      stream
      ->NodeJs.Stream.onData(chunk => {
        chan->Chan.emit(Stderr(NodeJs.Buffer.toString(chunk)))
        // store the latest message from stderr
        stderr := stderr.contents ++ NodeJs.Buffer.toString(chunk)
      })
      ->ignore
    )

    // on `close` from `stdin` or `process`
    let promiseOnClose = Promise.make((resolve, _) => {
      process
      ->NodeJs.ChildProcess.stdin
      ->Option.forEach(stream =>
        stream
        ->NodeJs.Stream.Writable.onClose(() => resolve(0))
        ->ignore
      )

      process
      ->NodeJs.ChildProcess.onClose(code => resolve(code))
      ->ignore
    })

    // on errors and anomalies
    let promiseOnExit = Promise.make((resolve, _) => {
      process
      ->NodeJs.ChildProcess.onExit(code => resolve(code))
      ->ignore
    })

    process
    ->NodeJs.ChildProcess.onDisconnect(() => chan->Chan.emit(Event(OnDestroyed)))
    ->NodeJs.ChildProcess.onError(exn =>
      chan->Chan.emit(Event(OnError(Util.JsError.toString(exn))))
    )
    ->ignore

    // emit `OnExit` when either `close` or `exit` was received
    Promise.race([promiseOnExit, promiseOnClose])
    ->Promise.thenResolve(exitCode => {
      chan->Chan.emit(Event(OnExit(exitCode)))
    })
    ->ignore

    {chan, status: Created(process)}
  }

  let destroy = self =>
    switch self.status {
    | Created(process) =>
      // set the status to "Destroying"
      // let (promise, resolve) = Promise.pending()
      let promise = Promise.make((resolve, _) => {
        // listen to the `exit` event
        let _ = self.chan->Chan.on(x =>
          switch x {
          | Event(OnExit(_)) =>
            self.chan->Chan.destroy
            self.status = Destroyed
            resolve()
          | _ => ()
          }
        )

        // trigger `exit`
        NodeJs.ChildProcess.kill(process, "SIGTERM")
      })
      self.status = Destroying(promise)
      promise
    | Destroying(promise) => promise
    | Destroyed => Promise.make((resolve, _) => resolve())
    }

  let send = (self, request): bool => {
    switch self.status {
    | Created(process) =>
      let payload = NodeJs.Buffer.fromString(request ++ NodeJs.Os.eol)
      process
      ->NodeJs.ChildProcess.stdin
      ->Option.forEach(stream =>
        stream
        ->NodeJs.Stream.Writable.write(payload)
        ->ignore
      )
      true
    | _ => false
    }
  }

  let onOutput = (self, callback) =>
    self.chan->Chan.on(output =>
      switch output {
      | Event(OnExit(_)) =>
        switch self.status {
        | Destroying(_) => () // triggered by `destroy`
        | _ => callback(output)
        }
      | _ => callback(output)
      }
    )
}

include Module
