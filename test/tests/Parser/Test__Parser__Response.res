open Mocha
open! Test__Parser__SExpression
open Test__Util

// [SExpression] -> [Response.Prioritized.t]
let toPrioritizedResponses = exprs =>
  // keeping the successful parsing result
  // Assert.fail on the failed ones
  exprs
  ->Array.map(Response.Prioritized.parse)
  ->Array.map(x =>
    switch x {
    | Error(e) =>
      Assert.fail(Parser.Error.toString(e))
      []
    | Ok(v) => [v]
    }
  )
  ->Array.flat

// parses a single wire line into one S-expression, failing the test if it doesn't
let parseSingle = wire =>
  switch Parser.SExpression.parse(wire) {
  | [Ok(sexpr)] => sexpr
  | _ =>
    Assert.fail("expected a single successfully parsed S-expression")
    Parser.SExpression.L([])
  }

describe("when parsing a response containing an escaped string value", () => {
  // #342: `Parser.SExpression.parse` now correctly decodes the doubled
  // backslash-n on the wire into literal backslash+n text (a string value's
  // own embedded newline), not a real line break -- but `Response.parse`
  // still runs several of its payloads through the now-redundant
  // `Parser.unescapeEOL`, which blindly turns ANY backslash+n text into a
  // real newline, silently undoing the fix for these exact cases. Each of
  // the 3 call sites in `Response.res` is a separately reachable corruption:
  // Agda can send a String value containing an embedded newline through any
  // of `*Goal type etc.*`-style info actions, `*Type-checking*` progress
  // messages, or `agda2-verbose` debug messages.
  it(
    "should preserve a string value's own embedded newline as literal text, not turn it into a real line break",
    () => {
      // the same real `*Goal type etc.*` response Agda 2.8.0 sends for
      //   test : "a\nb" ≡ "a\nb"
      // captured verbatim via `agda --interaction`.
      let wire = "(agda2-info-action \"*Goal type etc.*\" \"Goal: \\\"a\\\\nb\\\" ≡ \\\"a\\\\nb\\\"\\n————————————————————————————————————————————————————————————\" nil)"
      let actual = Response.parse(parseSingle(wire))
      let expected = Ok(
        Response.DisplayInfo(
          GoalType(
            "Goal: \"a\\nb\" ≡ \"a\\nb\"\n————————————————————————————————————————————————————————————",
          ),
        ),
      )
      Assert.deepStrictEqual(actual, expected)
    },
  )

  it(
    "should preserve an embedded newline in a *Type-checking* progress message",
    () => {
      // wire: (agda2-info-action "*Type-checking*" "x\\ny" t)
      let wire = "(agda2-info-action \"*Type-checking*\" \"x\\\\ny\" t)"
      let actual = Response.parse(parseSingle(wire))
      let expected = Ok(Response.RunningInfo(1, "x\\ny"))
      Assert.deepStrictEqual(actual, expected)
    },
  )

  it(
    "should preserve an embedded newline in an agda2-verbose message",
    () => {
      // wire: (agda2-verbose "x\\ny")
      let wire = "(agda2-verbose \"x\\\\ny\")"
      let actual = Response.parse(parseSingle(wire))
      let expected = Ok(Response.RunningInfo(2, "x\\ny"))
      Assert.deepStrictEqual(actual, expected)
    },
  )
})

describe("when parsing responses", () =>
  Golden.getGoldenFilepathsSync("../../../../test/tests/Parser/Response")->Array.forEach(filepath =>
    Async.it(
      "should golden test " ++ filepath,
      async () => {
        let raw = await Golden.readFile(filepath)
        raw
        ->Golden.map(parseSExpression([], ...))
        ->Golden.map(toPrioritizedResponses)
        ->Golden.map(Strings.unlinesWith(Response.Prioritized.toString, ...))
        ->Golden.compare
      },
    )
  )
)
