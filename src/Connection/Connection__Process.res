// module for communicating with a process
open Belt
// module for auto path searching
// module PathSearch = {
//   module Error = {
//     type t =
//       | ProcessHanging(string) // command name
//       | NotSupported(string) // OS name
//       | NotFound(string, string) // command name, error message

//     let toString = x =>
//       switch x {
//       | ProcessHanging(name) => (
//           "Process not responding when looking for \"" ++ (name ++ "\""),
//           j`Please restart the process`,
//         )
//       | NotSupported(os) => (
//           "Auto search failed",
//           j`currently auto path searching is not supported on $(os)`,
//         )
//       | NotFound(name, msg) => ("Auto search failed when looking for \"" ++ name ++ "\"", msg)
//       }
//   }

//   let run = (name, errMsgIfNotFound): Promise.t<result<string, Error.t>> => {
//     let (promise, resolve) = Promise.pending()

//     // reject if the process hasn't responded for more than 1 second
//     let hangTimeout = Js.Global.setTimeout(() => resolve(Error(Error.ProcessHanging(name))), 1000)

//     // the command we use for getting the path
//     let commandName = switch N.OS.type_() {
//     | "Linux"
//     | "Darwin" =>
//       Ok("which")
//     | "Windows_NT" => Ok("where.exe")
//     | os => Error(os)
//     }

//     switch commandName {
//     | Error(os) => resolve(Error(NotSupported(os)))
//     | Ok(commandName') =>
//       Nd.ChildProcess.exec(commandName' ++ (" " ++ name), (error, stdout, stderr) => {
//         // clear timeout as the process has responded
//         Js.Global.clearTimeout(hangTimeout)

//         // error
//         error
//         ->Js.Nullable.toOption
//         ->Option.forEach(err => {
//           let msg = switch Js.Exn.message(err) {
//           | None => errMsgIfNotFound
//           | Some(err) => errMsgIfNotFound ++ "\nError message from the system:\n" ++ err
//           }
//           resolve(Error(Error.NotFound(name, msg)))
//         })

//         // stderr
//         let stderr = Node.Buffer.toString(stderr)
//         if stderr != "" {
//           let msg = errMsgIfNotFound ++ "\nError message from stderr:\n" ++ stderr
//           resolve(Error(NotFound(name, msg)))
//         }

//         // stdout
//         let stdout = Node.Buffer.toString(stdout)->String.trim
//         if stdout == "" {
//           let msg = errMsgIfNotFound ++ "\nstdout is empty"
//           resolve(Error(NotFound(name, msg)))
//         } else {
//           resolve(Ok(stdout))
//         }
//       }) |> ignore
//     }

//     promise
//   }
// }

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
