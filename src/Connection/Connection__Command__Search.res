// module for searching executables in PATH with tools like `which` or `where.exe`

// Instead of returning the underlying error, we return `None` as a special case when `which` or `where.exe` is working correctly but the executable is not found.
let searchWithCommand = async (command, name, ~timeout=1000) => {
  switch await Connection__Process__Exec.run(command, [name], ~timeout) {
  | Ok(stdout) => Ok(String.trim(stdout)) // trim the string to remove the trailing newline
  | Error(FromStderr(Some(1), "")) => Error(None)
  | Error(error) => Error(Some(error))
  }
}

let search = async (name, ~timeout=1000) => {
  if OS.onUnix {
    await searchWithCommand("which", name, ~timeout)
  } else {
    // try `which` first, then `where.exe`
    switch await searchWithCommand("which", name, ~timeout) {
    | Ok(stdout) => Ok(stdout)
    | Error(_) => await searchWithCommand("where.exe", name, ~timeout)
    }
  }
}
