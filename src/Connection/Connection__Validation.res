module Process = Connection__Process

// module for validating a given path
module Error = {
  type t =
    | PathMalformed(string)
    | // the process has not been responding for some time
    ProcessHanging
    // error from the shell
    | NotFound(string)
    | ShellError(Js.Exn.t)
    // error from the process' stderr
    | FromStderr(string)
    // from the process' `error` event
    | FromOnError(string)
    // wrong invoked command
    | WrongProcess(string)
  let toString = x =>
    switch x {
    | PathMalformed(msg) => "path malformed: " ++ msg
    | ProcessHanging => "process hanging for more than 1 sec"

    | NotFound(path) => "command not found: " ++ path
    | ShellError(error) => "shell: " ++ Util.JsError.toString(error)
    | FromStderr(msg) => "stderr: " ++ msg
    | FromOnError(msg) => "on error: " ++ msg
    | WrongProcess(msg) => "wrong process: " ++ msg
    }
}

type output = string
type validator<'a> = output => result<'a, string>

// Handle error from the callback
let handleError = (command, error: Js.nullable<Js.Exn.t>): option<Error.t> =>
  error
  ->Js.Nullable.toOption
  ->Option.map(err => {
    let message = Option.getOr(Js.Exn.message(err), "")
    if (
      Js.Re.test_(%re("/command not found/"), message) ||
      Js.Re.test_(%re("/No such file or directory/"), message) ||
      Js.Re.test_(%re("/not found/"), message) ||
      String.endsWith(message, "ENOENT")
    ) {
      Error.NotFound(command)
    } else {
      ShellError(err)
    }
  })

let run3 = async (path, args): result<'a, Error.t> => {
  let process = Process.make(path, args)
  let (promise, resolve, _) = Util.Promise_.pending()

  let stdout = ref("")
  let stderr = ref("")

  let destructor = process->Process.onOutput(output =>
    switch output {
    | Process.Stdout(output) => stdout := stdout.contents ++ output
    | Process.Stderr(output) => stderr := stderr.contents ++ output
    | Process.Event(OnDestroyed) => resolve(Ok(stdout.contents))
    | Process.Event(OnExit(0)) => resolve(Ok(stdout.contents))
    | Process.Event(OnExit(127)) => resolve(Error(Error.NotFound(path)))
    | Process.Event(OnExit(_)) =>
      if stderr.contents != "" {
        if Process.errorMessageIndicatesNotFound(stderr.contents) {
          resolve(Error(Error.NotFound(path)))
        } else {
          resolve(Error(FromStderr(stderr.contents)))
        }
      } else {
        resolve(Ok(stdout.contents))
      }
    | Process.Event(OnError(msg)) =>
      if msg != "" {
        if Process.errorMessageIndicatesNotFound(msg) {
          resolve(Error(Error.NotFound(path)))
        } else {
          resolve(Error(FromOnError(msg)))
        }
      } else if stderr.contents != "" {
        if Process.errorMessageIndicatesNotFound(stderr.contents) {
          resolve(Error(Error.NotFound(path)))
        } else {
          resolve(Error(FromStderr(stderr.contents)))
        }
      } else {
        resolve(Ok(stdout.contents))
      }
    }
  )

  let result = await promise
  destructor()
  result
}

let run = (path, args): promise<result<'a, Error.t>> => {
  Promise.make((resolve, _) => {
    // the path must not be empty
    if path == "" {
      resolve(Error(Error.PathMalformed("the path must not be empty")))
    }

    // Special treatment for Windows:
    //  1. use cmd.exe to execute .bat files
    //  2. replace paths like "/c/path/to/agda" with "c:/path/to/agda"
    let (path, args) = if Util.onUnix {
      (path, args)
    } // Check if it's a .bat file
    else if String.endsWith(path, ".bat") {
      ("cmd.exe", ["/c", path, ...args])
    } else {
      (path, args)
    }

    // reject if the process hasn't responded for more than 20 second
    let hangTimeout = Js.Global.setTimeout(() => resolve(Error(ProcessHanging)), 20000)

    let command = Array.join([path, ...args], " ")
    NodeJs.ChildProcess.execWith(command, %raw(`{shell : true}`), (error, stdout, stderr) => {
      // clear timeout as the process has responded
      Js.Global.clearTimeout(hangTimeout)

      // handles `error` and rejects it if there's any
      handleError(command, error)->Option.forEach(err => resolve(Error(err)))

      // stderr
      let stderr = NodeJs.Buffer.toString(stderr)
      if stderr != "" {
        resolve(Error(FromStderr(stderr)))
      }

      // feed the stdout to the validator
      let stdout = NodeJs.Buffer.toString(stdout)
      resolve(Ok(stdout))
    })->ignore
  })
}

let run2 = (path, args, validator: validator<'a>): promise<result<'a, Error.t>> => {
  Promise.make((resolve, _) => {
    // the path must not be empty
    if path == "" {
      resolve(Error(Error.PathMalformed("the path must not be empty")))
    }

    // On Windows, we need to use cmd.exe to execute .bat files
    let (command, args) = if Util.onUnix {
      (path, args)
    } // Check if it's a .bat file
    else if String.endsWith(path, ".bat") {
      ("cmd.exe", ["/c", path, ...args])
    } else {
      (path, args)
    }

    // reject if the process hasn't responded for more than 20 second
    let hangTimeout = Js.Global.setTimeout(() => resolve(Error(ProcessHanging)), 20000)

    ignore(
      NodeJs.ChildProcess.execFile(command, args, (error, stdout, stderr) => {
        // clear timeout as the process has responded
        Js.Global.clearTimeout(hangTimeout)

        // handles `error` and rejects it if there's any
        handleError(command, error)->Option.forEach(err => resolve(Error(err)))

        // stderr
        let stderr = NodeJs.Buffer.toString(stderr)
        if stderr != "" {
          resolve(Error(FromStderr(stderr)))
        }

        // feed the stdout to the validator
        let stdout = NodeJs.Buffer.toString(stdout)
        switch validator(stdout) {
        | Error(err) => resolve(Error(WrongProcess(err)))
        | Ok(result) => resolve(Ok(result))
        }
      }),
    )
  })
}
