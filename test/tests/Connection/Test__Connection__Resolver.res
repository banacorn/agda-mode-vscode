open Mocha

module Resolver = Connection.Resolver

describe("Path Searching", () => {
  This.timeout(10000)

  // describe("`Source.search` with `FromFile`", () => {
  //   Async.before(
  //     async () => {
  //       let file = await NodeJs.Fs.open_("temp", NodeJs.Fs.Flag.write)
  //       await NodeJs.Fs.FileHandle.writeFile(file, NodeJs.Buffer.fromString("test"))
  //     },
  //   )

  //   Async.it(
  //     "for file that exists",
  //     async () => {
  //       switch await Resolver.search(FromFile("temp"), ~timeout=1000) {
  //       | Error(err) => Exn.raiseError(Resolver.Error.toString(err))
  //       | Ok(ViaPipe(command, args, options, source)) =>
  //         Assert.deepEqual(command, "temp")
  //         Assert.deepEqual(args, [])
  //         Assert.deepEqual(options, None)
  //         Assert.deepEqual(source, FromFile("temp"))
  //       | Ok(ViaTCP(_)) => Exn.raiseError("Expected ViaCommand")
  //       }
  //     },
  //   )

  //   Async.it(
  //     "for file that doesn't exist",
  //     async () => {
  //       switch await Resolver.search(FromFile("temp-non-existing"), ~timeout=1000) {
  //       | Error(error) => Assert.deepEqual(error, File("temp-non-existing"))
  //       | Ok(ViaPipe(_)) => Exn.raiseError("Expected Error")
  //       | Ok(ViaTCP(_)) => Exn.raiseError("Expected Error")
  //       }
  //     },
  //   )

  //   Async.after(
  //     async () => {
  //       NodeJs.Fs.unlinkSync("temp")
  //     },
  //   )
  // })

  // describe("`Source.search` with `FromCommand`", () => {
  //   Async.it(
  //     "for command that exists",
  //     async () => {
  //       switch await Resolver.search(FromCommand("which")) {
  //       | Error(err) => Exn.raiseError(Resolver.Error.toString(err))
  //       | Ok(ViaPipe(command, args, options, source)) =>
  //         let path = switch await Resolver.Command.search("which") {
  //         | Error(err) => Js.Exn.raiseError(Resolver.Command.Error.toString(err))
  //         | Ok(path) => path
  //         }

  //         Assert.deepEqual(command, path)
  //         Assert.deepEqual(args, [])
  //         Assert.deepEqual(options, None)
  //         Assert.deepEqual(source, FromCommand("which"))
  //       | Ok(ViaTCP(_)) => Exn.raiseError("Expected ViaCommand")
  //       }
  //     },
  //   )

  //   Async.it(
  //     "for command that doesn't exist",
  //     async () => {
  //       switch await Resolver.search(FromCommand("temp-non-existing")) {
  //       | Error(error) => Assert.deepEqual(error, Command("temp-non-existing", NotFound))
  //       | Ok(ViaPipe(_)) => Exn.raiseError("Expected Error")
  //       | Ok(ViaTCP(_)) => Exn.raiseError("Expected Error")
  //       }
  //     },
  //   )
  // })

  // describe("`Source.search` with `FromTCP`", () => {
  //   open NodeJs.Net

  //   let serverRef = ref(None)

  //   Async.before(
  //     async () => {
  //       let server = TcpServer.make()
  //       server->TcpServer.listen(~port=23456, ~host="localhost", ~callback=_ => ())->ignore
  //       serverRef := Some(server)
  //     },
  //   )

  //   Async.it(
  //     "for TCP server that exists",
  //     async () => {
  //       switch await Resolver.search(
  //         FromTCP(NodeJs.Url.make("lsp://localhost:23456")),
  //         ~timeout=5000,
  //       ) {
  //       | Error(err) => Exn.raiseError(Resolver.Error.toString(err))
  //       | Ok(ViaPipe(_)) => Exn.raiseError("Expected ViaTCP")
  //       | Ok(ViaTCP(url, source)) =>
  //         Assert.deepEqual(url.port, 23456)
  //         Assert.deepEqual(url.hostname, "localhost")
  //         Assert.deepEqual(source, FromTCP(NodeJs.Url.make("lsp://localhost:23456")))
  //       }
  //     },
  //   )

  //   Async.it(
  //     "for local TCP server that doesn't exist",
  //     async () => {
  //       switch await Resolver.search(
  //         FromTCP(NodeJs.Url.make("lsp://localhost:23457")),
  //         ~timeout=5000,
  //       ) {
  //       | Error(TCP(url, error)) =>
  //         Assert.deepEqual(url.port, 23457)
  //         Assert.deepEqual(url.hostname, "localhost")
  //         Assert.deepEqual(Resolver.TCP.Error.toString(error), "AggregateError")
  //       | Error(_) => raise(Failure("Expecting TCP-related error"))
  //       | Ok(ViaPipe(_)) => Exn.raiseError("Expecting Error")
  //       | Ok(ViaTCP(_)) => Exn.raiseError("Expecting Error")
  //       }
  //     },
  //   )

  //   Async.it(
  //     "for remote TCP server that doesn't exist",
  //     async () => {
  //       switch await Resolver.search(
  //         FromTCP(NodeJs.Url.make("lsp://remotehost:23458")),
  //         ~timeout=5000,
  //       ) {
  //       | Error(TCP(url, error)) =>
  //         Assert.deepEqual(url.port, 23458)
  //         Assert.deepEqual(url.hostname, "remotehost")
  //         let result = await Resolver.GitHub.Platform.determine()
  //         switch result["os"] {
  //         | "darwin" =>
  //           Assert.deepEqual(
  //             Resolver.TCP.Error.toString(error),
  //             "Error: getaddrinfo ENOTFOUND remotehost",
  //           )
  //         | "win32" =>
  //           Assert.deepEqual(
  //             Resolver.TCP.Error.toString(error),
  //             "Error: getaddrinfo ENOTFOUND remotehost",
  //           )
  //         | "linux" =>
  //           if result["dist"] == "Ubuntu" {
  //             Assert.deepEqual(
  //               Resolver.TCP.Error.toString(error),
  //               "Error: getaddrinfo EAI_AGAIN remotehost",
  //             )
  //           }
  //         | _ => ()
  //         }
  //       | Error(_) => raise(Failure("Expecting TCP-related error"))
  //       | Ok(ViaPipe(_)) => Exn.raiseError("Expecting Error")
  //       | Ok(ViaTCP(_)) => Exn.raiseError("Expecting Error")
  //       }
  //     },
  //   )

  //   Async.after(
  //     async () => {
  //       switch serverRef.contents {
  //       | Some(server) =>
  //         TcpServer.close(server, ~callback=_ => ())->ignore
  //         serverRef := None
  //       | None => Exn.raiseError("Server not found")
  //       }
  //     },
  //   )
  // })

  // skip when API rate limit is reached
  // describe_skip("`Source.search` with `FromGitHub`", () => {
  //   open Resolver.GitHub
  //   // so that we can delete the whole directory after the test
  //   let downloadDirRef = ref(None)

  //   let afterDownload = async (isCached, (path, target)) => {
  //     // include "Agda_datadir" in the environment variable
  //     let options = {
  //       let assetPath = NodeJs.Path.join2(path, "data")
  //       let env = Js.Dict.fromArray([("Agda_datadir", assetPath)])
  //       {
  //         Connection__Target__ALS__LSP__Binding.env: env,
  //       }
  //     }
  //     // chmod the executable after download
  //     // no need to chmod if:
  //     //    1. it's cached, already chmoded
  //     //  or
  //     //    2. it's on Windows
  //     let execPath = NodeJs.Path.join2(path, "als")
  //     let shouldChmod = !isCached && NodeJs.Os.platform() != "win32"
  //     if shouldChmod {
  //       let _ = await chmodExecutable(execPath)
  //     }

  //     // store the download path for cleanup
  //     downloadDirRef := Some(path)

  //     Ok((execPath, [], Some(options), target))
  //   }

  //   let repo = {
  //     Repo.username: "agda",
  //     repository: "agda-language-server",
  //     userAgent: "agda/agda-mode-vscode",
  //     memento: State__Type.Memento.make(None),
  //     globalStoragePath: "./",
  //     cacheInvalidateExpirationSecs: 86400,
  //   }

  //   let callbacks = {
  //     Callbacks.chooseFromReleases: releases =>
  //       switch releases->Release.chooseByTagName("v0.2.6.4.0.3") {
  //       | None => None
  //       | Some(release) =>
  //         switch release.assets->Asset.chooseByName("als-windows.zip") {
  //         | None => None
  //         | Some(asset) =>
  //           Some({
  //             release,
  //             asset,
  //             saveAsFileName: release.tag_name ++ "-als-windows.zip",
  //           })
  //         }
  //       },
  //     onDownload: _ => (),
  //     afterDownload,
  //     log: x => Js.log(x),
  //   }

  //   Async.it(
  //     "download v0.2.6.4.0.3 from GitHub the first time",
  //     async () => {
  //       // set timeout to 600 seconds because we are downloading stuff for the first time
  //       This.timeout(600000)

  //       switch await Resolver.search(Resolver.FromGitHub(repo, callbacks)) {
  //       | Error(err) => Exn.raiseError(Resolver.Error.toString(err))
  //       | Ok(ViaPipe(command, args, options, source)) =>
  //         let downloadDir = switch downloadDirRef.contents {
  //         | Some(path) => path
  //         | None => Exn.raiseError("Expected download path")
  //         }

  //         // `command` should be the path to the download directory + "/als"
  //         Assert.deepEqual(command, NodeJs.Path.join2(downloadDir, "als"))
  //         // no arguments supplied in this test case
  //         Assert.deepEqual(args, [])
  //         // `options` should include "Agda_datadir" in the environment variable
  //         let expectedOptions = Some({
  //           Connection__Target__ALS__LSP__Binding.env: Dict.fromArray([
  //             ("Agda_datadir", NodeJs.Path.join2(downloadDir, "data")),
  //           ]),
  //         })
  //         Assert.deepEqual(options, expectedOptions)

  //         switch source {
  //         | FromGitHub(repo, release, _) =>
  //           Assert.deepEqual(repo.username, "agda")
  //           Assert.deepEqual(repo.repository, "agda-language-server")
  //           Assert.deepEqual(repo.userAgent, "agda/agda-mode-vscode")
  //           Assert.deepEqual(repo.globalStoragePath, "./")
  //           Assert.deepEqual(release.tag_name, "v0.2.6.4.0.3")
  //         | _ => Exn.raiseError("Expected FromGitHub")
  //         }

  //       | Ok(ViaTCP(_)) => Exn.raiseError("Expected ViaPipe")
  //       }
  //     },
  //   )

  //   Async.it(
  //     "download v0.2.6.4.0.3 from GitHub the second time",
  //     async () => {
  //       // set timeout to only 1 seconds because we are downloading stuff for the second time
  //       This.timeout(1000)
  //       switch await Resolver.search(Resolver.FromGitHub(repo, callbacks), ~timeout=1000) {
  //       | Error(err) => Exn.raiseError(Resolver.Error.toString(err))
  //       | Ok(ViaPipe(command, args, options, source)) =>
  //         let downloadDir = switch downloadDirRef.contents {
  //         | Some(path) => path
  //         | None => Exn.raiseError("Expected download path")
  //         }

  //         // `command` should be the path to the download directory + "/als"
  //         Assert.deepEqual(command, NodeJs.Path.join2(downloadDir, "als"))
  //         // no arguments supplied in this test case
  //         Assert.deepEqual(args, [])
  //         // `options` should include "Agda_datadir" in the environment variable
  //         let expectedOptions = Some({
  //           Connection__Target__ALS__LSP__Binding.env: Dict.fromArray([
  //             ("Agda_datadir", NodeJs.Path.join2(downloadDir, "data")),
  //           ]),
  //         })
  //         Assert.deepEqual(options, expectedOptions)

  //         switch source {
  //         | FromGitHub(repo, release, _) =>
  //           Assert.deepEqual(repo.username, "agda")
  //           Assert.deepEqual(repo.repository, "agda-language-server")
  //           Assert.deepEqual(repo.userAgent, "agda/agda-mode-vscode")
  //           Assert.deepEqual(repo.globalStoragePath, "./")
  //           Assert.deepEqual(release.tag_name, "v0.2.6.4.0.3")
  //         | _ => Exn.raiseError("Expected FromGitHub")
  //         }

  //       | Ok(ViaTCP(_)) => Exn.raiseError("Expected ViaPipe")
  //       }
  //     },
  //   )

  //   Async.after(
  //     async () => {
  //       // remove the cache file and the download file
  //       try {
  //         NodeJs.Fs.unlinkSync("releases-cache.json")
  //         NodeJs.Fs.unlinkSync("in-flight.download")
  //       } catch {
  //       | _ => ()
  //       }
  //       switch downloadDirRef.contents {
  //       | Some(path) =>
  //         await Resolver.GitHub.Nd.Fs.rmWithOptions(path, {recursive: true, force: true})
  //       | None => ()
  //       }
  //     },
  //   )
  // })
})

// describe("Port Probing", () => {
//   This.timeout(10000)
//   describe("`Source.Port.probe`", () => {
//     Async.it(
//       "should report Ok on the port that is available",
//       async () => {
//         let tempServer = NodeJs.Net.TcpServer.make()
//         await Promise.make(
//           (resolve, _) => {
//             tempServer
//             ->NodeJs.Net.TcpServer.listen(~port=23456, ~host="localhost", ~callback=resolve)
//             ->ignore
//           },
//         )

//         let _ = await Source.TCP.probe(23456, "localhost", ~timeout=1000)
//         NodeJs.Net.TcpServer.close(tempServer, ~callback=_ => ())->ignore
//       },
//     )

//     Async.it(
//       "should report Error on ports that are not available",
//       async () => {
//         switch await Source.TCP.probe(12345, "localhost", ~timeout=1000) {
//         | Error(_exn) => ()
//         | Ok() => raise(Js.Exn.raiseError("Port should not be available"))
//         }
//       },
//     )
//   })
// })
