open! BsMocha.Mocha
open Test__Util

describe_skip("Connection", ~timeout=10000, () => {
  Q.it("should download the language server", () => {
    let globalStoragePath = Path.globalStoragePath()
    let useLSP = true

    Connection.start(globalStoragePath, useLSP, _ => ())
    ->Promise.mapOk(_ => ())
    ->Promise.flatMapError(e => {
      let (header, body) = Connection.Error.toString(e)
      let message = header ++ "\n" ++ body
      A.fail(message)
    })
  })
})
