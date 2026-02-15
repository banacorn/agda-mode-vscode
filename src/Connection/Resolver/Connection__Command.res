// module for searching executables in PATH with tools like `which` or `where.exe`

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

// Instead of returning the underlying error, we return `None` as a special case when `which` or `where.exe` is working correctly but the executable is not found.
let searchWith = async (command, name, ~timeout=1000) => {
  switch await Connection__Process__Exec.run(command, [name], ~timeout) {
  | Ok(stdout) =>
    let path = String.trim(stdout) // trim the string to remove the trailing newline
    Ok(path)
  | Error(FromStderr(Some(1), "")) => Error(Error.NotFound)
  | Error(FromStderr(Some(1), "INFO: Could not find files for the given pattern(s).\r\n")) =>
    Error(NotFound)
  | Error(error) => Error(SomethingWentWrong(error))
  }
}

let search = async (name, ~timeout=1000) => {
  // sometimes GitHub Actions runner exits with code 0 but does not write to stdout;
  // this function retries a few times
  let flakyWhich = () => {
    let rec retry = async (n: int) =>
      switch await searchWith("which", name, ~timeout) {
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

  if OS.onUnix {
    await flakyWhich()
  } else {
    // try `which` first, then `where.exe`
    switch await flakyWhich() {
    | Ok(stdout) => Ok(stdout)
    | Error(_) => await searchWith("where.exe", name, ~timeout)
    }
  }
}
