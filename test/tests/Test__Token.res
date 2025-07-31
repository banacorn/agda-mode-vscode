open Mocha

describe("Token", () => {
  describe("Token.parse", () => {
    it(
      "should return None for atomic SExpression",
      () => {
        let result = Token.parse(Parser.SExpression.A("test"))
        Assert.deepStrictEqual(result, None)
      },
    )

    it(
      "should parse basic token without source",
      () => {
        let sexp = Parser.SExpression.L([A("10"), A("20"), L([A("keyword")])])
        let result = Token.parse(sexp)
        let expected = Some({
          Token.start: 9, // 10 - 1
          end: 19, // 20 - 1
          aspects: [Highlighting__AgdaAspect.Keyword],
          isTokenBased: false,
          note: None,
          source: None,
        })
        Assert.deepStrictEqual(result, expected)
      },
    )

    it(
      "should parse token with extra field",
      () => {
        let sexp = Parser.SExpression.L([A("5"), A("15"), L([A("function")]), A("extra")])
        let result = Token.parse(sexp)
        let expected = Some({
          Token.start: 4, // 5 - 1
          end: 14, // 15 - 1
          aspects: [Highlighting__AgdaAspect.Function],
          isTokenBased: false,
          note: None,
          source: None,
        })
        Assert.deepStrictEqual(result, expected)
      },
    )

    it(
      "should parse token with source information",
      () => {
        let sexp = Parser.SExpression.L([
          A("1"),
          A("5"),
          L([A("module")]),
          A(""),
          A(""),
          L([A("/path/to/file.agda"), A(""), A("42")]),
        ])
        let result = Token.parse(sexp)
        let expected = Some({
          Token.start: 0, // 1 - 1
          end: 4, // 5 - 1
          aspects: [Highlighting__AgdaAspect.Module],
          isTokenBased: false,
          note: None,
          source: Some((Parser.Filepath.make("/path/to/file.agda"), 42)),
        })
        Assert.deepStrictEqual(result, expected)
      },
    )

    it(
      "should parse token with multiple aspects",
      () => {
        let sexp = Parser.SExpression.L([A("100"), A("110"), L([A("function"), A("operator")])])
        let result = Token.parse(sexp)
        let expected = Some({
          Token.start: 99, // 100 - 1
          end: 109, // 110 - 1
          aspects: [Highlighting__AgdaAspect.Function, Highlighting__AgdaAspect.Operator],
          isTokenBased: false,
          note: None,
          source: None,
        })
        Assert.deepStrictEqual(result, expected)
      },
    )

    it(
      "should return None for invalid start offset",
      () => {
        let sexp = Parser.SExpression.L([A("invalid"), A("20"), L([A("keyword")])])
        let result = Token.parse(sexp)
        Assert.deepStrictEqual(result, None)
      },
    )

    it(
      "should return None for invalid end offset",
      () => {
        let sexp = Parser.SExpression.L([A("10"), A("invalid"), L([A("keyword")])])
        let result = Token.parse(sexp)
        Assert.deepStrictEqual(result, None)
      },
    )

    it(
      "should return None for invalid source index",
      () => {
        let sexp = Parser.SExpression.L([
          A("1"),
          A("5"),
          L([A("module")]),
          A(""),
          A(""),
          L([A("/path/to/file.agda"), A(""), A("invalid")]),
        ])
        let result = Token.parse(sexp)
        Assert.deepStrictEqual(result, None)
      },
    )

    it(
      "should return None for malformed SExpression",
      () => {
        let sexp = Parser.SExpression.L([A("only_one_element")])
        let result = Token.parse(sexp)
        Assert.deepStrictEqual(result, None)
      },
    )
  })

  describe("Token.parseDirectHighlightings", () => {
    it(
      "should parse empty array",
      () => {
        let result = Token.parseDirectHighlightings([])
        Assert.deepStrictEqual(result, [])
      },
    )

    it(
      "should skip first two elements and parse rest",
      () => {
        let tokens = [
          Parser.SExpression.A("skip1"),
          A("skip2"),
          L([A("10"), A("20"), L([A("keyword")])]),
          L([A("30"), A("40"), L([A("function")])]),
        ]
        let result = Token.parseDirectHighlightings(tokens)
        let expected = [
          {
            Token.start: 9, // 10 - 1
            end: 19, // 20 - 1
            aspects: [Highlighting__AgdaAspect.Keyword],
            isTokenBased: false,
            note: None,
            source: None,
          },
          {
            Token.start: 29, // 30 - 1
            end: 39, // 40 - 1
            aspects: [Highlighting__AgdaAspect.Function],
            isTokenBased: false,
            note: None,
            source: None,
          },
        ]
        Assert.deepStrictEqual(result, expected)
      },
    )

    it(
      "should filter out invalid tokens",
      () => {
        let tokens = [
          Parser.SExpression.A("skip1"),
          A("skip2"),
          L([A("10"), A("20"), L([A("keyword")])]), // valid
          L([A("invalid"), A("40"), L([A("function")])]), // invalid
          L([A("50"), A("60"), L([A("symbol")])]), // valid
        ]
        let result = Token.parseDirectHighlightings(tokens)
        let expected = [
          {
            Token.start: 9, // 10 - 1
            end: 19, // 20 - 1
            aspects: [Highlighting__AgdaAspect.Keyword],
            isTokenBased: false,
            note: None,
            source: None,
          },
          {
            Token.start: 49, // 50 - 1
            end: 59, // 60 - 1
            aspects: [Highlighting__AgdaAspect.Symbol],
            isTokenBased: false,
            note: None,
            source: None,
          },
        ]
        Assert.deepStrictEqual(result, expected)
      },
    )
  })

  describe("Token.decodeToken", () => {
    it(
      "should decode valid JSON token",
      () => {
        open JsonCombinators.Json.Decode
        let json = JSON.parseExn(`[10, 20, ["keyword"], false, null, null]`)
        let result = decode(json, Token.decodeToken)
        switch result {
        | Ok(token) =>
          Assert.deepStrictEqual(token.start, 9) // 10 - 1
          Assert.deepStrictEqual(token.end, 19) // 20 - 1
          Assert.deepStrictEqual(token.aspects, [Highlighting__AgdaAspect.Keyword])
          Assert.deepStrictEqual(token.isTokenBased, false)
          Assert.deepStrictEqual(token.note, None)
          Assert.deepStrictEqual(token.source, None)
        | Error(err) => Assert.fail("Decoding should succeed: " ++ err)
        }
      },
    )

    it(
      "should decode token with source",
      () => {
        open JsonCombinators.Json.Decode
        let json = JSON.parseExn(`[5, 15, ["module"], true, "note", ["/path/file.agda", 100]]`)
        let result = decode(json, Token.decodeToken)
        switch result {
        | Ok(token) =>
          Assert.deepStrictEqual(token.start, 4) // 5 - 1
          Assert.deepStrictEqual(token.end, 14) // 15 - 1
          Assert.deepStrictEqual(token.aspects, [Highlighting__AgdaAspect.Module])
          Assert.deepStrictEqual(token.isTokenBased, true)
          Assert.deepStrictEqual(token.note, Some("note"))
          Assert.deepStrictEqual(token.source, Some((Parser.Filepath.make("/path/file.agda"), 100)))
        | Error(err) => Assert.fail("Decoding should succeed: " ++ err)
        }
      },
    )

    it(
      "should handle multiple aspects",
      () => {
        open JsonCombinators.Json.Decode
        let json = JSON.parseExn(`[1, 10, ["function", "operator"], false, null, null]`)
        let result = decode(json, Token.decodeToken)
        switch result {
        | Ok(token) =>
          Assert.deepStrictEqual(
            token.aspects,
            [Highlighting__AgdaAspect.Function, Highlighting__AgdaAspect.Operator],
          )
        | Error(err) => Assert.fail("Decoding should succeed: " ++ err)
        }
      },
    )
  })

  describe("Token.decodeResponseHighlightingInfoDirect", () => {
    it(
      "should decode highlighting response",
      () => {
        open JsonCombinators.Json.Decode
        let json = JSON.parseExn(`[true, [[10, 20, ["keyword"], false, null, null]]]`)
        let result = decode(json, Token.decodeResponseHighlightingInfoDirect)
        switch result {
        | Ok((keepHighlighting, tokens)) =>
          Assert.deepStrictEqual(keepHighlighting, true)
          let expectedTokens = [
            {
              Token.start: 9, // 10 - 1
              end: 19, // 20 - 1
              aspects: [Highlighting__AgdaAspect.Keyword],
              isTokenBased: false,
              note: None,
              source: None,
            },
          ]
          Assert.deepStrictEqual(tokens, expectedTokens)
        | Error(err) => Assert.fail("Decoding should succeed: " ++ err)
        }
      },
    )

    it(
      "should decode empty token array",
      () => {
        open JsonCombinators.Json.Decode
        let json = JSON.parseExn(`[false, []]`)
        let result = decode(json, Token.decodeResponseHighlightingInfoDirect)
        switch result {
        | Ok((keepHighlighting, tokens)) =>
          Assert.deepStrictEqual(keepHighlighting, false)
          Assert.deepStrictEqual(tokens, [])
        | Error(err) => Assert.fail("Decoding should succeed: " ++ err)
        }
      },
    )
  })

  describe("Token.toString and toStringWithoutOffsets", () => {
    let sampleToken = {
      Token.start: 10,
      end: 20,
      aspects: [Highlighting__AgdaAspect.Function, Highlighting__AgdaAspect.Operator],
      isTokenBased: false,
      note: None,
      source: Some((Parser.Filepath.make("/test.agda"), 42)),
    }

    it(
      "should format token with offsets",
      () => {
        let result = Token.toString(sampleToken)
        Assert.ok(String.includes(result, "(10, 20)"))
        Assert.ok(String.includes(result, "Function"))
        Assert.ok(String.includes(result, "[src: 42]"))
      },
    )

    it(
      "should format token without offsets",
      () => {
        let result = Token.toStringWithoutOffsets(sampleToken)
        Assert.ok(!String.includes(result, "(10, 20)"))
        Assert.ok(String.includes(result, "Function"))
        Assert.ok(String.includes(result, "[src: 42]"))
      },
    )

    it(
      "should handle token without source",
      () => {
        let tokenNoSource = {...sampleToken, source: None}
        let result = Token.toStringWithoutOffsets(tokenNoSource)
        Assert.ok(!String.includes(result, "[src:"))
      },
    )

    it(
      "should handle empty aspects",
      () => {
        let tokenNoAspects = {...sampleToken, aspects: []}
        let result = Token.toString(tokenNoAspects)
        Assert.ok(String.includes(result, "(10, 20)"))
      },
    )
  })
})
