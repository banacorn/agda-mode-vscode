module Command = Connection__Resolver__Command
module File = Connection__Resolver__File
module TCP = Connection__Resolver__TCP
module GitHub = Connection__Resolver__GitHub
module IPC = Connection__IPC


// error from the sources
// module Error = {
//   type t =
//     | File(string) // path of the program
//     | Command(string, Command.Error.t) // name of the command, error
//     | TCP(NodeJs.Url.t, TCP.Error.t)
//     | GitHub(GitHub.Error.t)

//   let toString = error =>
//     switch error {
//     | File(path) => "Trying to locate \"" ++ path ++ "\" but the file does not exist"
//     | Command(name, e) =>
//       "Trying to find the command \"" ++ name ++ "\": " ++ Command.Error.toString(e)
//     | TCP(url, e) =>
//       "Trying to connect to " ++ url.toString() ++ " but got " ++ TCP.Error.toString(e)
//     | GitHub(e) => "Trying to download prebuilt from GitHub: " ++ GitHub.Error.toString(e)
//     }
// }

module Module: {
  // returns `IPC.t` if any is found, and errors of previous searches
  // let search: (t, ~timeout: int=?) => promise<result<IPC.t, Error.t>>
  // let searchMany: array<t> => promise<(option<IPC.t>, array<Error.t>)>
} = {
  // returns the method of IPC if successful
  // let search = async (source, ~timeout=1000) =>
  //   switch source {
  //   | FromFile(path) =>
  //     if File.probe(path) {
  //       Ok(IPC.ViaPipe(path, [], None, FromFile(path)))
  //     } else {
  //       Error(Error.File(path))
  //     }
  //   | FromCommand(name) =>
  //     switch await Command.search(name, ~timeout) {
  //     | Error(e) => Error(Error.Command(name, e))
  //     | Ok(path) => Ok(IPC.ViaPipe(path, [], None, FromCommand(name)))
  //     }
  //   | FromTCP(url) =>
  //     switch await TCP.probe(url, ~timeout) {
  //     | Error(e) => Error(Error.TCP(url, e))
  //     | Ok() => Ok(IPC.ViaTCP(url, FromTCP(url)))
  //     }
  //   | FromGitHub(repo, callbacks) =>
  //     switch await GitHub.get(repo, callbacks) {
  //     | Error(e) => Error(Error.GitHub(e))
  //     | Ok((isCached, target)) =>
  //       let destPath = NodeJs.Path.join2(repo.globalStoragePath, target.saveAsFileName)
  //       // switch await callbacks.afterDownload(isCached, (destPath, target)) {
  //       // | Error(e) => Error(Error.GitHub(e))
  //       // | Ok((path, args, options, target)) =>
  //       //   Ok(IPC.ViaPipe(path, args, options, FromGitHub(repo, target.release, target.asset)))
  //       // }
  //     }
  //   }

  // let searchMany = async sources => {
  //   let rec tryUntilSuccess = async (accumErrors: list<Error.t>, input) =>
  //     switch input {
  //     | list{} => (None, list{})
  //     | list{x} =>
  //       switch await search(x) {
  //       | Error(e) => (None, list{e})
  //       | Ok(v) => (Some(v), list{})
  //       }
  //     | list{x, ...xs} =>
  //       switch await search(x) {
  //       | Error(e) =>
  //         let (v, es) = await tryUntilSuccess(accumErrors, xs)
  //         (v, list{e, ...es})
  //       | Ok(v) => (Some(v), accumErrors)
  //       }
  //     }
  //   let (client, errors) = await tryUntilSuccess(list{}, sources->List.fromArray)
  //   (client, List.toArray(errors))
  // }
}

include Module
