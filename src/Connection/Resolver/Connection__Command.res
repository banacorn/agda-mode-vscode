// module for searching executables in PATH with tools like `command -v` or `where.exe`

module Error = {
  type t =
    | NotFound
    | InternalError
    | SomethingWentWrong(Connection__Process__Exec.Error.t)

  let toString = error =>
    switch error {
    | NotFound => "Command not found"
    | InternalError => "Internal error"
    | SomethingWentWrong(e) => Connection__Process__Exec.Error.toString(e)
    }
}

// Instead of returning the underlying error, we return `None` as a special case when
// command lookup is working correctly but the executable is not found.
//
// A `ProcessHanging` result doesn't mean the executable is missing, only that this
// particular attempt didn't respond in time (e.g. a loaded CI runner), so it's retried
// a bounded number of times with a short backoff before being reported as an error.
let searchWith = async (command, args, ~timeout=5000, ~retries=2, ~backoff=200) => {
  let rec attempt = async n =>
    switch await Connection__Process__Exec.run(command, args, ~timeout, ~shell=false) {
    | Ok(stdout) =>
      let path = String.trim(stdout) // trim the string to remove the trailing newline
      Ok(path)
    | Error(FromStderr(Some(1), "")) => Error(Error.NotFound)
    | Error(FromStderr(Some(1), "INFO: Could not find files for the given pattern(s).\r\n")) =>
      Error(NotFound)
    | Error(ProcessHanging(_) as error) =>
      if n > 0 {
        await Util.Promise_.setTimeout(backoff)
        await attempt(n - 1)
      } else {
        Error(SomethingWentWrong(error))
      }
    | Error(error) => Error(SomethingWentWrong(error))
    }
  await attempt(retries)
}

let search = async (name, ~timeout=5000) => {
  // sometimes GitHub Actions runner exits with code 0 but does not write to stdout;
  // this function retries a few times
  let flakyLookup = () => {
    let rec retry = async (n: int) =>
      switch await searchWith("sh", ["-c", "command -v -- \"$1\"", "--", name], ~timeout) {
      | Ok("") => {
        if n > 0 {
          await retry(n - 1)
        } else {
          Error(Error.InternalError)
        }
      }
      | result => result
    }
    retry(3)
  }

  let filterPath = result =>
    switch result {
    | Ok(path) =>
      let trimmed = path->String.trim
      let hasSeparator = String.includes(trimmed, "/") || String.includes(trimmed, "\\")
      let isAbsolute = NodeJs.Path.isAbsolute(trimmed)
      if trimmed != "" && (isAbsolute || hasSeparator) {
        Ok(trimmed)
      } else {
        Error(Error.NotFound)
      }
    | Error(error) => Error(error)
    }

  if OS.onUnix {
    filterPath(await flakyLookup())
  } else {
    // use `where.exe` directly on Windows
    filterPath(await searchWith("where.exe", [name], ~timeout))
  }
}
