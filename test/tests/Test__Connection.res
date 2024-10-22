open Mocha
open Test__Util

describe_skip("Connection", () => {
  Async.it("should download the language server", async () => {
    let globalStoragePath = Path.globalStoragePath
    let useLSP = true

    switch await Connection.start(globalStoragePath, useLSP, _ => ()) {
    | Ok(_) => Assert.ok(true)
    | Error(e) =>
      let (header, body) = Connection.Error.toString(e)
      let message = header ++ "\n" ++ body
      raise(Failure(message))
    }
  })
})
