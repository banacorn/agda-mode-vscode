open Mocha

describe("when running Parser.escape", () => {
  it("should make escaped backslash explicit", () => {
    let raw = "\\ x -> x"
    let expected = "\\\\ x -> x"
    let actual = Parser.escape(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should make escaped newline on Unix explicit", () => {
    let raw = "x\ny"
    let expected = "x\\ny"
    let actual = Parser.escape(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should make escaped newline on Windows explicit", () => {
    let raw = "x\r\ny"
    let expected = "x\\r\\ny"
    let actual = Parser.escape(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should make escaped double quote explicit", () => {
    let raw = "\"x\""
    let expected = "\\\"x\\\""
    let actual = Parser.escape(raw)
    Assert.deepStrictEqual(actual, expected)
  })
})

describe("when running Parser.unescapeEOL", () => {
  it("should make explicit newline on Unix implicit", () => {
    let raw = "x\\ny"
    let expected = "x\ny"
    let actual = Parser.unescapeEOL(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should make explicit newline on Windows implicit", () => {
    let raw = "x\\r\\ny"
    let expected = "x\r\ny"
    let actual = Parser.unescapeEOL(raw)
    Assert.deepStrictEqual(actual, expected)
  })
})
