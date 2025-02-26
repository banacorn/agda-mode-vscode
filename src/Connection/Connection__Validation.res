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
    | ProcessError(string)
    // wrong invoked command
    | WrongProcess(string)
  let toString = x =>
    switch x {
    | PathMalformed(msg) => "path malformed: " ++ msg
    | ProcessHanging => "process hanging for more than 1 sec"

    | NotFound(path) => "command not found: " ++ path
    | ShellError(error) => "shell: " ++ Util.JsError.toString(error)
    | ProcessError(msg) => "stderr: " ++ msg
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

let run = (path, args, validator: validator<'a>): promise<result<'a, Error.t>> => {
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
        resolve(Error(ProcessError(stderr)))
      }

      // feed the stdout to the validator
      let stdout = NodeJs.Buffer.toString(stdout)
      switch validator(stdout) {
      | Error(err) => resolve(Error(WrongProcess(err)))
      | Ok(result) => resolve(Ok(result))
      }
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
          resolve(Error(ProcessError(stderr)))
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
