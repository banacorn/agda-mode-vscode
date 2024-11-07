open Mocha

describe("when running Parser.userInputToSExpr", () => {
  it("should make escaped backslash explicit", () => {
    let raw = "\\ x -> x"
    let expected = "\\\\ x -> x"
    let actual = Parser.userInputToSExpr(raw)
    Assert.deepEqual(actual, expected)
  })

  it("should make escaped newline on Unix explicit", () => {
    let raw = "x\ny"
    let expected = "x\\ny"
    let actual = Parser.userInputToSExpr(raw)
    Assert.deepEqual(actual, expected)
  })

  it("should make escaped newline on Windows explicit", () => {
    let raw = "x\r\ny"
    let expected = "x\\r\\ny"
    let actual = Parser.userInputToSExpr(raw)
    Assert.deepEqual(actual, expected)
  })

  it("should make escaped double quote explicit", () => {
    let raw = "\"x\""
    let expected = "\\\"x\\\""
    let actual = Parser.userInputToSExpr(raw)
    Assert.deepEqual(actual, expected)
  })

})