// module for communicating with a process
open Belt
// module for auto path searching
module PathSearch = {
  module Error = {
    type t =
      | ProcessHanging(string) // command name
      | NotSupported(string) // OS name
      | NotFound(string, string) // command name, error message

    let toString = x =>
      switch x {
      | ProcessHanging(name) => (
          "Process not responding when looking for \"" ++ (name ++ "\""),
          j`Please restart the process`,
        )
      | NotSupported(os) => (
          "Auto search failed",
          j`currently auto path searching is not supported on $(os)`,
        )
      | NotFound(name, msg) => (
          "Auto search failed when looking for \"" ++ (name ++ "\""),
          j`If you know where the executable of Agda is located, please fill it in "agdaMode.agdaPath" in the Settings.
The system responded with the following message $(msg)`,
        )
      }
  }

  let run = (name): Promise.t<result<string, Error.t>> => {
    let (promise, resolve) = Promise.pending()

    // reject if the process hasn't responded for more than 1 second
    let hangTimeout = Js.Global.setTimeout(() => resolve(Error(Error.ProcessHanging(name))), 1000)

    // the command we use for getting the path
    let commandName = switch N.OS.type_() {
    | "Linux"
    | "Darwin" =>
      Ok("which")
    | "Windows_NT" => Ok("where.exe")
    | os => Error(os)
    }

    switch commandName {
    | Error(os) => resolve(Error(NotSupported(os)))
    | Ok(commandName') =>
      Nd.ChildProcess.exec(commandName' ++ (" " ++ name), (error, stdout, stderr) => {
        // clear timeout as the process has responded
        Js.Global.clearTimeout(hangTimeout)

        // error
        error
        ->Js.Nullable.toOption
        ->Option.forEach(err =>
          resolve(Error(Error.NotFound(name, Option.getWithDefault(Js.Exn.message(err), ""))))
        )

        // stderr
        let stderr = Node.Buffer.toString(stderr)
        if stderr != "" {
          resolve(Error(NotFound(name, stderr)))
        }

        // stdout
        let stdout = Node.Buffer.toString(stdout)->String.trim
        if stdout == "" {
          resolve(Error(NotFound(name, "")))
        } else {
          resolve(Ok(stdout))
        }
      }) |> ignore
    }

    promise
  }
}

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
      | PathMalformed(msg) => ("Path malformed", msg)
      | ProcessHanging => (
          "Process hanging",
          "The program has not been responding for more than 1 sec",
        )
      | NotFound(error) => ("Command not found", Util.JsError.toString(error))
      | ShellError(error) => ("Error from the shell", Util.JsError.toString(error))
      | ProcessError(msg) => ("Error from the stderr", msg)
      | WrongProcess(msg) => ("Wrong process", msg)
      }
  }

  type output = string
  type validator<'a> = output => result<'a, string>

  let run = (path, validator: validator<'a>): Promise.t<result<'a, Error.t>> => {
    // parsing the parse error
    let parseError = (error: Js.Nullable.t<Js.Exn.t>): option<Error.t> =>
      error
      ->Js.Nullable.toOption
      ->Option.map(err => {
        let message = Option.getWithDefault(Js.Exn.message(err), "")
        if Js.Re.test_(%re("/No such file or directory/"), message) {
          Error.NotFound(err)
        } else if Js.Re.test_(%re("/command not found/"), message) {
          NotFound(err)
        } else {
          ShellError(err)
        }
      })

    let (promise, resolve) = Promise.pending()

    // the path must not be empty
    if path == "" {
      resolve(Error(Error.PathMalformed("the path must not be empty")))
    }

    // reject if the process hasn't responded for more than 20 second
    let hangTimeout = Js.Global.setTimeout(() => resolve(Error(ProcessHanging)), 20000)

    Nd.ChildProcess.exec(path, (error, stdout, stderr) => {
      // clear timeout as the process has responded
      Js.Global.clearTimeout(hangTimeout)

      // parses `error` and rejects it if there's any
      parseError(error)->Belt.Option.forEach(err => resolve(Error(err)))

      // stderr
      let stderr = Node.Buffer.toString(stderr)
      if stderr != "" {
        resolve(Error(ProcessError(stderr)))
      }

      // feed the stdout to the validator
      let stdout = Node.Buffer.toString(stdout)
      switch validator(stdout) {
      | Error(err) => resolve(Error(WrongProcess(err)))
      | Ok(result) => resolve(Ok(result))
      }
    }) |> ignore

    promise
  }
}

module Error = {
  type exitCode = int
  type signal = string
  type t =
    | ClosedByProcess(exitCode, signal) // on `close`
    | DisconnectedByUser // on `disconnect
    | ShellError(Js.Exn.t) // on `error`
    | ExitedByProcess(exitCode, signal, string) // on 'exit`
    | NotEstablishedYet

  let toString = x =>
    switch x {
    | ClosedByProcess(code, signal) => (
        "Socket closed by process",
        j`exited with code: $code
signal: $signal
`,
      )
    | DisconnectedByUser => ("Disconnected", "Connection disconnected by ourselves")
    | ShellError(error) => ("Socket error", Util.JsError.toString(error))
    | ExitedByProcess(code, signal, stderr) => (
        "Agda has crashed !",
        j`exited with code: $code
  signal: $signal
  === message from stderr ===
  $stderr
  `,
      )

    | NotEstablishedYet => (
        "Connection not established yet",
        "Please establish the connection first",
      )
    }
}

module type Module = {
  type output =
    | Stdout(string)
    | Stderr(string)
    | Error(Error.t)

  type t
  // lifetime: same as the child process
  let make: (string, array<string>) => t
  let destroy: t => Promise.t<unit>

  let send: (t, string) => result<unit, Error.t>
  let onOutput: (t, output => unit, unit) => unit
  let isConnected: t => bool
}
module Module: Module = {
  type output =
    | Stdout(string)
    | Stderr(string)
    | Error(Error.t)

  type status =
    | Connected(Nd.ChildProcess.t)
    | Disconnecting(Promise.t<unit>)
    | Disconnected

  type t = {
    chan: Chan.t<output>,
    mutable status: status,
    mutable forcedExit: bool,
  }

  let make = (path, args): t => {
    let chan = Chan.make()
    let stderr = ref("")
    // spawn the child process
    let process = Nd.ChildProcess.spawn_(
      "\"" ++ (path ++ "\""),
      args,
      Nd.ChildProcess.spawnOption(~shell=Nd.ChildProcess.Shell.bool(true), ()),
    )

    // on `data` from `stdout`
    process
    |> Nd.ChildProcess.stdout
    |> Nd.Stream.Readable.on(
      #data(chunk => chan->Chan.emit(Stdout(Node.Buffer.toString(chunk))) |> ignore),
    )
    |> ignore

    // on `data` from `stderr`
    process
    |> Nd.ChildProcess.stderr
    |> Nd.Stream.Readable.on(
      #data(
        chunk => {
          chan->Chan.emit(Stderr(Node.Buffer.toString(chunk))) |> ignore
          // store the latest message from stderr
          stderr := Node.Buffer.toString(chunk)
        },
      ),
    )
    |> ignore

    // on `close` from `stdin`
    process
    |> Nd.ChildProcess.stdin
    |> Nd.Stream.Writable.on(
      #close(() => chan->Chan.emit(Error(Error.ClosedByProcess(0, ""))) |> ignore),
    )
    |> ignore

    // on errors and anomalies
    process
    |> Nd.ChildProcess.on(
      #close((code, signal) => chan->Chan.emit(Error(ClosedByProcess(code, signal))) |> ignore),
    )
    |> Nd.ChildProcess.on(#disconnect(() => chan->Chan.emit(Error(DisconnectedByUser)) |> ignore))
    |> Nd.ChildProcess.on(#error(exn => chan->Chan.emit(Error(ShellError(exn))) |> ignore))
    |> Nd.ChildProcess.on(
      #exit(
        (code, signal) =>
          if code != 0 {
            //  returns the last message from stderr
            chan->Chan.emit(Error(ExitedByProcess(code, signal, stderr.contents))) |> ignore
          },
      ),
    )
    |> ignore

    {chan: chan, status: Connected(process), forcedExit: false}
  }

  let destroy = self =>
    switch self.status {
    | Connected(process) =>
      // set the status to "Disconnecting"
      let (promise, resolve) = Promise.pending()
      self.status = Disconnecting(promise)

      self.forcedExit = true

      // listen to the `exit` event
      self.chan->Chan.on(x =>
        switch x {
        | Error(ExitedByProcess(_, _, _)) =>
          self.chan->Chan.destroy
          self.status = Disconnected
          resolve()
        | _ => ()
        }
      ) |> ignore

      // trigger `exit`
      Nd.ChildProcess.kill_("SIGTERM", process) |> ignore

      // resolve on `exit`
      promise
    | Disconnecting(promise) => promise
    | Disconnected => Promise.resolved()
    }

  let send = (self, request): result<unit, Error.t> =>
    switch self.status {
    | Connected(process) =>
      let payload = Node.Buffer.fromString(request ++ "\n")
      // write
      process |> Nd.ChildProcess.stdin |> Nd.Stream.Writable.write(payload) |> ignore

      Ok()
    | _ => Error(Error.NotEstablishedYet)
    }

  let onOutput = (self, callback) =>
    self.chan->Chan.on(output =>
      switch output {
      | Error(Error.ExitedByProcess(_, _, _)) =>
        if self.forcedExit {
          self.forcedExit = false
        } else {
          callback(output)
        }
      | _ => callback(output)
      }
    )

  let isConnected = self =>
    switch self.status {
    | Connected(_) => true
    | _ => false
    }
}

include Module
