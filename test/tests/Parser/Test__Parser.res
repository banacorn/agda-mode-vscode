open Mocha

describe("Parser.escape", () => {
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

describe("Parser.unescapeEOL", () => {
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

describe("Parser.Filepath", () => {
  it("should remove Windows Bidi control characters", () => {
    let actual = Parser.Filepath.make("\u202A/path/to/file.agda")
    let expected = Parser.Filepath.make("/path/to/file.agda")
    Assert.deepStrictEqual(actual, expected)
  })

  it("should normalize paths", () => {
    let actual = Parser.Filepath.make("/path/./to/../file.agda")
    let expected = Parser.Filepath.make("/path/file.agda")
    Assert.deepStrictEqual(actual, expected)
  })

  it("should be neutral regaring separators (backslash vs slash)", () => {
    let actual = Parser.Filepath.make("C:\\path\\to\\file.agda")
    let expected = Parser.Filepath.make("C:/path/to/file.agda")
    Assert.deepStrictEqual(actual, expected)
  })

  if OS.onUnix {
    it("should not remove roots on Unix", () => {
      let actual = Parser.Filepath.make("/path/dir/file.txt")->Parser.Filepath.toString
      let expected = "/path/dir/file.txt"
      Assert.deepStrictEqual(actual, expected)
    })
  } else {
    it("should convert small case roots to upper case on Windows", () => {
      let actual = Parser.Filepath.make("c:\\path\\dir\\file.txt")
      let expected = Parser.Filepath.make("C:\\path\\dir\\file.txt")
      Assert.deepStrictEqual(actual, expected)
    })
  }
})

describe("Parser.filepath", () => {
  it("should remove Windows Bidi control characters", () => {
    let raw = "\u202A/path/to/file.agda"
    let expected = "/path/to/file.agda"
    let actual = Parser.filepath(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should normalize the path", () => {
    let raw = "/path/./to/../file.agda"
    let expected = "/path/file.agda"
    let actual = Parser.filepath(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should replace Windows backslash with slash", () => {
    let raw = "C:\\path\\to\\file.agda"
    let expected = "C:/path/to/file.agda"
    let actual = Parser.filepath(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  if !OS.onUnix {
    it("should convert small case Windows roots to upper case", () => {
      let raw = "c:\\path\\dir\\file.txt"
      let expected = "C:/path/dir/file.txt"
      let actual = Parser.filepath(raw)
      Assert.deepStrictEqual(actual, expected)
    })
  }
})
