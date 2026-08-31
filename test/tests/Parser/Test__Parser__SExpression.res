open Mocha
open Test__Util

// [Int] -> String -> [SExpression]
let parseSExpression = (breakpoints, input) => {
  open Parser.Incr.Gen

  let output = ref([])

  let parser = Parser.SExpression.makeIncr(x =>
    switch x {
    | Yield(Error((errNo, raw))) => Assert.fail(Parser.Error.toString(SExpression(errNo, raw)))
    | Yield(Ok(a)) => output.contents->Array.push(a)
    | Stop => ()
    }
  )

  input
  ->String.trim
  ->Strings.breakInput(breakpoints)
  ->Array.map(Parser.splitToLines)
  ->Array.flat
  ->Array.forEach(Parser.Incr.feed(parser, ...))

  output.contents
}

describe("when parsing an escaped string atom", () => {
  // #342: Agda's wire protocol (`Agda.Utils.String.quote` in Agda's own
  // source) escapes a *real* newline character as a single backslash
  // ("\n"), but a `String` *value* whose own content already contains a
  // literal "\n" as text (because Haskell's `Show` instance for `String`
  // escaped it first) gets that backslash escaped *again* by `quote`,
  // producing a doubled "\\n" on the wire. `Parser.SExpression` must tell
  // these apart -- a lone backslash before "n" is Agda's own structural
  // marker and should decode to a real newline; a *paired* run of
  // backslashes decodes to that many literal backslash characters, with
  // the trailing "n" left as ordinary text.
  it("should decode a lone escaped newline into a real newline", () => {
    // wire: (foo "x\ny")  -- one backslash before "n"
    let wire = "(foo \"x\\ny\")"
    let actual = Parser.SExpression.parse(wire)
    let expected = [Ok(Parser.SExpression.L([A("foo"), A("x\ny")]))]
    Assert.deepStrictEqual(actual, expected)
  })

  it("should decode a doubled escaped backslash-n as literal text", () => {
    // wire: (foo "x\\ny")  -- two backslashes before "n"
    let wire = "(foo \"x\\\\ny\")"
    let actual = Parser.SExpression.parse(wire)
    let expected = [Ok(Parser.SExpression.L([A("foo"), A("x\\ny")]))]
    Assert.deepStrictEqual(actual, expected)
  })

  it("should decode a single escaped quote as literal content, not close the string", () => {
    // wire: (foo "a\"b")  -- one backslash before a quote mid-string.
    // Agda's `quote` only backslash-escapes a `"` that is itself part of the
    // string's own content, so a lone backslash before `"` must decode to a
    // literal quote character and must NOT toggle out of the string (the
    // string only ends at the final, unescaped `"`).
    let wire = "(foo \"a\\\"b\")"
    let actual = Parser.SExpression.parse(wire)
    let expected = [Ok(Parser.SExpression.L([A("foo"), A("a\"b")]))]
    Assert.deepStrictEqual(actual, expected)
  })

  it("should decode a doubled escaped backslash landing right before the closing quote", () => {
    // wire: (foo "ab\\")  -- two backslashes immediately followed by the
    // string's closing quote, with no ordinary character in between. This
    // happens whenever a displayed string value's content itself ends in a
    // literal backslash character (Show + quote double-escape it), and that
    // string is the last thing before the delimiter -- nothing in Agda's
    // algorithm requires an ordinary character to separate the two, so the
    // pending-run resolution has to correctly fall through into the normal
    // closing-quote handling for the same character that ended the run.
    let wire = "(foo \"ab\\\\\")"
    let actual = Parser.SExpression.parse(wire)
    let expected = [Ok(Parser.SExpression.L([A("foo"), A("ab\\")]))]
    Assert.deepStrictEqual(actual, expected)
  })

  it("should decode two doubled escaped backslashes landing right before the closing quote", () => {
    // wire: (foo "ab\\\\")  -- four backslashes immediately followed by the
    // closing quote, e.g. raw (non-Show) content ending in two literal
    // backslash characters, such as a Windows-style path fragment. Same
    // boundary interaction as the single-pair case above, but proves the
    // pairs arithmetic (not just parity) carries through correctly when the
    // run is longer than one pair.
    let wire = "(foo \"ab\\\\\\\\\")"
    let actual = Parser.SExpression.parse(wire)
    let expected = [Ok(Parser.SExpression.L([A("foo"), A("ab\\\\")]))]
    Assert.deepStrictEqual(actual, expected)
  })

  it("should decode a doubled escaped backslash-r as literal text", () => {
    // wire: (foo "x\\r\\ny")  -- two backslashes before "r" and two before "n",
    // mirroring the real wire bytes captured for a string value containing a
    // literal CRLF (`"a\r\nb"`), confirmed byte-for-byte via `od -c`.
    let wire = "(foo \"x\\\\r\\\\ny\")"
    let actual = Parser.SExpression.parse(wire)
    let expected = [Ok(Parser.SExpression.L([A("foo"), A("x\\r\\ny")]))]
    Assert.deepStrictEqual(actual, expected)
  })

  it("should decode a run of three backslashes before \"n\" as one literal backslash plus a real newline", () => {
    // wire: (foo "x\\\ny")  -- three backslashes before "n". This is what
    // Agda produces when a string value's content ends in a literal
    // backslash character (Show + quote double-escape it to two wire
    // backslashes) immediately followed, with no separator, by Agda's own
    // structural single-backslash newline marker: 2 content backslashes +
    // 1 structural backslash = 3, decoding to one literal backslash
    // (the content pair) plus a real newline (the structural marker).
    let wire = "(foo \"x\\\\\\ny\")"
    let actual = Parser.SExpression.parse(wire)
    let expected = [Ok(Parser.SExpression.L([A("foo"), A("x\\\ny")]))]
    Assert.deepStrictEqual(actual, expected)
  })

  // The real `*Goal type etc.*` response Agda 2.8.0 sends for the goal
  //   test : "a\nb" ≡ "a\nb"
  // (a String value containing a literal newline), captured verbatim via
  // `agda --interaction` -- not a synthetic stand-in. Agda's own display
  // must survive on one line, exactly as `quote` escaped it: the string's
  // embedded newline stays literal "\n" text inside the quotes, and only
  // the structural line break before the "————" delimiter becomes a real
  // newline.
  it("should decode a real Agda response for a string containing a newline", () => {
    let wire = "(agda2-info-action \"*Goal type etc.*\" \"Goal: \\\"a\\\\nb\\\" ≡ \\\"a\\\\nb\\\"\\n————————————————————————————————————————————————————————————\" nil)"
    let actual = Parser.SExpression.parse(wire)
    let expected = [
      Ok(
        Parser.SExpression.L([
          A("agda2-info-action"),
          A("*Goal type etc.*"),
          A("Goal: \"a\\nb\" ≡ \"a\\nb\"\n————————————————————————————————————————————————————————————"),
          A("nil"),
        ]),
      ),
    ]
    Assert.deepStrictEqual(actual, expected)
  })
})

describe("when parsing S-expressions as a whole", () =>
  Golden.getGoldenFilepathsSync(
    "../../../../test/tests/Parser/SExpression",
  )->Array.forEach(filepath =>
    Async.it(
      "should golden test " ++ filepath,
      async () => {
        let raw = await Golden.readFile(filepath)
        raw
        ->Golden.map(parseSExpression([], ...))
        ->Golden.map(Strings.unlinesWith(Parser.SExpression.toString, ...))
        ->Golden.compare
      },
    )
  )
)

describe("when parsing S-expressions incrementally", () =>
  Golden.getGoldenFilepathsSync(
    "../../../../test/tests/Parser/SExpression",
  )->Array.forEach(filepath =>
    Async.it(
      "should golden test " ++ filepath,
      async () => {
        let raw = await Golden.readFile(filepath)
        raw
        ->Golden.map(parseSExpression([3, 23, 171, 217, 1234, 2342, 3453], ...))
        ->Golden.map(Strings.unlinesWith(Parser.SExpression.toString, ...))
        ->Golden.compare
      },
    )
  )
)
