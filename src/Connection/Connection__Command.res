// module for searching executables in PATH with tools like `which` or `where.exe`

module Error = {
  type t =
    | NotFound(string)
    | SomethingWentWrong(string, Connection__Process__Exec.Error.t)
    | NotValidTarget(string, string, Connection__Target.Error.t)

  let toString = error =>
    switch error {
    | NotFound(command) => "Cannot find `" ++ command ++ "` in PATH"
    | SomethingWentWrong(command, e) =>
      "Cannot find `" ++
      command ++
      "` because: " ++
      Connection__Process__Exec.Error.toString(e) ++ "."
    | NotValidTarget(command, path, e) =>
      "Found `" ++
      command ++
      "` at `" ++
      path ++
      "` but it's not a valid target because: " ++
      Connection__Target.Error.toString(e)
    }
}

// Instead of returning the underlying error, we return `None` as a special case when `which` or `where.exe` is working correctly but the executable is not found.
let searchWith = async (command, name, ~timeout=1000) => {
  switch await Connection__Process__Exec.run(command, [name], ~timeout) {
  | Ok(stdout) =>
    let path = String.trim(stdout) // trim the string to remove the trailing newline
    switch await Connection__Target.fromRawPath(path) {
    | Ok(target) => Ok(target)
    | Error(error) => Error(Error.NotValidTarget(name, path, error))
    }
  | Error(FromStderr(Some(1), "")) => Error(NotFound(name))
  | Error(FromStderr(Some(1), "INFO: Could not find files for the given pattern(s).\r\n")) => Error(NotFound(name))
  | Error(error) => Error(SomethingWentWrong(name, error))
  }
}

let search = async (name, ~timeout=1000) => {
  if OS.onUnix {
    await searchWith("which", name, ~timeout)
  } else {
    // try `which` first, then `where.exe`
    switch await searchWith("which", name, ~timeout) {
    | Ok(stdout) => Ok(stdout)
    | Error(_) => await searchWith("where.exe", name, ~timeout)
    }
  }
}
