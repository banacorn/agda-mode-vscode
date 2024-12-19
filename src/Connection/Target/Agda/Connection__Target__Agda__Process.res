// module for communicating with a process

// module for validating a given path
module Validation = {
  module Error = {
    type t =
      | PathMalformed(string)
      | // the process has not been responding for some time
      ProcessHanging
      // error from the shell
      | NotFound(Js.Exn.t)
      | ShellError(Js.Exn.t)
      // error from the process' stderr
      | ProcessError(string)
      // wrong invoked command
      | WrongProcess(string)
    let toString = x =>
      switch x {
      | PathMalformed(msg) => "Path malformed: " ++ msg
      | ProcessHanging => "Process hanging: the program has not been responding for more than 1 sec"

      | NotFound(error) => "Command not found: " ++ Util.JsError.toString(error)
      | ShellError(error) => "Error from the shell: " ++ Util.JsError.toString(error)
      | ProcessError(msg) => "Error from the stderr: " ++ msg
      | WrongProcess(msg) => "Wrong process: " ++ msg
      }
  }

  type output = string
  type validator<'a> = output => result<'a, string>

  let run = (path, args, validator: validator<'a>): promise<result<'a, Error.t>> => {
    // parsing the parse error
    let parseError = (error: Js.nullable<Js.Exn.t>): option<Error.t> =>
      error
      ->Js.Nullable.toOption
      ->Option.map(err => {
        let message = Option.getOr(Js.Exn.message(err), "")
        if Js.Re.test_(%re("/No such file or directory/"), message) {
          Error.NotFound(err)
        } else if Js.Re.test_(%re("/command not found/"), message) {
          NotFound(err)
        } else {
          ShellError(err)
        }
      })

    Promise.make((resolve, _) => {
      // the path must not be empty
      if path == "" {
        resolve(Error(Error.PathMalformed("the path must not be empty")))
      }

      // reject if the process hasn't responded for more than 20 second
      let hangTimeout = Js.Global.setTimeout(() => resolve(Error(ProcessHanging)), 20000)

      // clear timeout as the process has responded

      // parses `error` and rejects it if there's any

      // stderr

      // feed the stdout to the validator

      ignore(
        NodeJs.ChildProcess.execFile(path, args, (error, stdout, stderr) => {
          Js.Global.clearTimeout(hangTimeout)

          parseError(error)->Belt.Option.forEach(err => resolve(Error(err)))

          let stderr = NodeJs.Buffer.toString(stderr)
          if stderr != "" {
            resolve(Error(ProcessError(stderr)))
          }

          let stdout = NodeJs.Buffer.toString(stdout)
          switch validator(stdout) {
          | Error(err) => resolve(Error(WrongProcess(err)))
          | Ok(result) => resolve(Ok(result))
          }
        }),
      )
    })
  }
}

module Event = {
  type exitCode = int
  type signal = string
  type path = string
  type args = array<string>
  type t =
    | OnDestroyed // on `disconnect` (destroyed by the user)
    | OnError(Js.Exn.t) // on `error`
    | OnExit(path, args, exitCode, string) // on `exit` or `close`

  let toString = x =>
    switch x {
    | OnDestroyed => "Process destroyed"
    | OnError(error) => Util.JsError.toString(error)
    | OnExit(_, _, code, "") => "Process exited with code " ++ string_of_int(code)
    | OnExit(_, _, code, stderr) =>
      "Process exited with code " ++ string_of_int(code) ++ "\n" ++ stderr
    }
}

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
        stderr := NodeJs.Buffer.toString(chunk)
      })
      ->ignore
    )

    // on `close` from `stdin` or `process`
    let promiseOnClose = Promise.make((resolve, _) => {
      process
      ->NodeJs.ChildProcess.stdin
      ->Option.forEach(stream =>
        stream
        ->NodeJs.Stream.Writable.onClose(() => resolve((path, args, 0, stderr.contents)))
        ->ignore
      )

      process
      ->NodeJs.ChildProcess.onClose(code => resolve((path, args, code, stderr.contents)))
      ->ignore
    })

    // on errors and anomalies
    let promiseOnExit = Promise.make((resolve, _) => {
      process
      ->NodeJs.ChildProcess.onExit(code => resolve((path, args, code, stderr.contents)))
      ->ignore
    })

    process
    ->NodeJs.ChildProcess.onDisconnect(() => chan->Chan.emit(Event(OnDestroyed)))
    ->NodeJs.ChildProcess.onError(exn => chan->Chan.emit(Event(OnError(exn))))
    ->ignore

    // emit `OnExit` when either `close` or `exit` was received
    Promise.race([promiseOnExit, promiseOnClose])
    ->Promise.thenResolve(((path, args, exitCode, stderr)) => {
      chan->Chan.emit(Event(OnExit(path, args, exitCode, stderr)))
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
          | Event(OnExit(_, _, _, _)) =>
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
      | Event(OnExit(_, _, _, _)) =>
        switch self.status {
        | Destroying(_) => () // triggered by `destroy`
        | _ => callback(output)
        }
      | _ => callback(output)
      }
    )
}

include Module
