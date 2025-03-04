module Process = Connection__Process

// module for validating a given path
module Error = {
  type t =
    | PathMalformed(string)
    | // the process has not been responding for 20 seconds
    ProcessHanging
    // error from the shell
    | NotFound(string)
    // error from the process' stderr
    | FromStderr(string)
    // from the process' `error` event
    | FromOnError(string)
  // wrong invoked command
  let toString = x =>
    switch x {
    | PathMalformed(msg) => "path malformed: " ++ msg
    | ProcessHanging => "process hanging for more than 1 sec"

    | NotFound(path) => "command not found: " ++ path
    | FromStderr(msg) => "stderr: " ++ msg
    | FromOnError(msg) => "on error: " ++ msg
    }
}

let run = async (path, args): result<'a, Error.t> => {
  let process = Process.make(path, args)
  let (promise, resolve, _) = Util.Promise_.pending()

  // the path must not be empty
  if path == "" {
    resolve(Error(Error.PathMalformed("the path must not be empty")))
  }

  // reject if the process hasn't responded for more than 20 second
  let hangTimeout = ref(
    Some(Js.Global.setTimeout(() => resolve(Error(Error.ProcessHanging)), 20000)),
  )

  let stdout = ref("")
  let stderr = ref("")
  let destructor = process->Process.onOutput(output => {
    // clear timeout as the process has responded
    switch hangTimeout.contents {
    | Some(timeout) => Js.Global.clearTimeout(timeout)
    | None => ()
    }

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
  })

  let result = await promise
  destructor()
  result
}
