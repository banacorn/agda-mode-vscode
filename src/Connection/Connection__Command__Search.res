// module for searching executables in PATH
module Error = {
  type t =
    // | NotResponding // the searching takes more than 1 second
    | NotSupported(string) // with OS name
    | OnError(Js.Exn.t)
    | OnError2(Connection__Process__Exec.Error.t)
    | OnStderr(string)
    | NotFound

  let toString = x =>
    switch x {
    // | NotResponding => "Took more than 1 second to when looking for the executable"
    | NotSupported(os) => "Path searching is not supported on \"" ++ os ++ "\""
    | OnError(exn) => "Got error when looking for the executable: " ++ Util.JsError.toString(exn)
    | OnError2(err) =>
      "Got error when looking for the executable: " ++ Connection__Process__Exec.Error.toString(err)
    | OnStderr(msg) => "Got something from the stderr when looking for the executable: " ++ msg
    | NotFound => "Cannot find the executable on PATH"
    }
}

// search for the executable in PATH with the given tool like `which` or `where.exe`
let searchWithCommand = async (command, name): result<string, Error.t> => {
  switch await Connection__Process__Exec.run(command, [name]) {
  | Ok(output) => Ok(String.trim(output))
  | Error(error) => Error(Error.OnError2(error))
  }
}

// search for the executable in PATH
let search = (name, ~timeout=1000) => {
  let timeout = async () => {
    await Util.Promise_.setTimeout(timeout)
    Error(Error.NotFound)
  }

  let search = async () =>
    switch NodeJs.Os.type_() {
    | "Linux"
    | "Darwin" =>
      await searchWithCommand("which", name)
    | "Windows_NT" =>
      // try `which` first, then `where.exe`
      switch await searchWithCommand("which", name) {
      | Ok(stdout) => Ok(stdout)
      | Error(_) => await searchWithCommand("where.exe", name)
      }
    | os => Error(NotSupported(os))
    }

  Promise.race([search(), timeout()])
}
