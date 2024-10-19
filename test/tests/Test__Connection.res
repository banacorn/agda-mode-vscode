open Mocha
open Test__Util

describe_skip("Connection", () => {
  Q.it("should download the language server", async () => {
    let globalStoragePath = Path.globalStoragePath
    let useLSP = true

    switch await Connection.start(globalStoragePath, useLSP, _ => ()) {
    | Ok(_) => Ok()
    | Error(e) =>
      let (header, body) = Connection.Error.toString(e)
      let message = header ++ "\n" ++ body
      await A.fail(message)
    }
  })
})
