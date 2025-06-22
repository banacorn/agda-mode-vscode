open Mocha
open Test__Util

open Tokens

describe("Tokens", () => {
  This.timeout(10000)
  describe("Token generation", () => {
    Async.it(
      "should produce 28 tokens",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")
        let tokens =
          ctx.state.tokens
          ->toArray
          ->Array.map(
            token => {
              let range = VSCode.Range.make(
                VSCode.TextDocument.positionAt(ctx.state.document, token.start),
                VSCode.TextDocument.positionAt(ctx.state.document, token.end),
              )
              Editor.Range.toString(range) ++ " " ++ Token.toString(token)
            },
          )
        Assert.deepStrictEqual(Array.length(tokens), 28)
      },
    )

    Async.it(
      "should produce correct tokens",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")
        let tokens =
          ctx.state.tokens
          ->toArray
          ->Array.map(
            token => {
              let range = VSCode.Range.make(
                VSCode.TextDocument.positionAt(ctx.state.document, token.start),
                VSCode.TextDocument.positionAt(ctx.state.document, token.end),
              )
              Editor.Range.toString(range) ++ " " ++ Token.toStringWithoutOffsets(token)
            },
          )
        let srcOfPrimitive = switch ctx.state.agdaVersion {
        | Some(version) =>
          if Util.Version.gte(version, "2.6.4") {
            "[src: 388]"
          } else {
            "[src: 320]"
          }
        | None => raise(Failure("No Agda version found"))
        }

        Assert.deepStrictEqual(
          tokens,
          [
            "0:0-6 [Keyword]",
            "0:7-21 [Module] [src: 1]",
            "0:22-27 [Keyword]",
            "1:0-4 [Keyword]",
            "1:5-6 [Datatype] [src: 34]",
            "1:7-8 [Symbol]",
            "1:9-12 [Primitive] " ++ srcOfPrimitive,
            "1:13-18 [Keyword]",
            "2:2-3 [ConstructorInductive] [src: 50]",
            "2:4-5 [Symbol]",
            "2:6-7 [Datatype] [src: 34]",
            "3:2-3 [ConstructorInductive] [src: 58]",
            "3:4-5 [Symbol]",
            "3:6-7 [Datatype] [src: 34]",
            "3:8-9 [Symbol]",
            "3:10-11 [Datatype] [src: 34]",
            "5:0-3 [Function, Operator] [src: 69]",
            "5:4-5 [Symbol]",
            "5:6-7 [Datatype] [src: 34]",
            "5:8-9 [Symbol]",
            "5:10-11 [Datatype] [src: 34]",
            "5:12-13 [Symbol]",
            "5:14-15 [Datatype] [src: 34]",
            "6:0-1 [Bound] [src: 85]",
            "6:2-3 [Function, Operator] [src: 69]",
            "6:4-5 [Bound] [src: 89]",
            "6:6-7 [Symbol]",
            "6:8-15 [Hole]",
          ],
        )
      },
    )
  })

  describe("`goToDefinition`", () => {
    Async.it(
      "should return the position of the definition",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Lib.agda")
        let filepath = ctx.state.document->VSCode.TextDocument.fileName->Parser.Filepath.make
        let position = VSCode.Position.make(12, 27)

        switch Tokens.goToDefinition(ctx.state.tokens, ctx.state.document)(filepath, position) {
        | None => raise(Failure("No definition found for the given position"))
        | Some(thunk) =>
          let actual = await thunk
          let expected = [
            (
              VSCode.Range.make(VSCode.Position.make(12, 26), VSCode.Position.make(12, 27)),
              filepath->Parser.Filepath.toString,
              VSCode.Position.make(12, 22),
            ),
          ]
          Assert.deepStrictEqual(actual, expected)
        }
      },
    )
  })

  describe("Change", () => {
    open FastCheck
    open Property.Sync

    it(
      "`arbitraryBatch` should generate valid changes",
      () => {
        assert_(property1(Change.arbitraryBatch(), xs => Change.areValid(xs)))
      },
    )
  })

  describe("Intervals", () => {
    open FastCheck
    open Property.Sync
    it(
      "`empty` should should be valid",
      () => {
        Assert.deepStrictEqual(Intervals.empty->Intervals.hasError, None)
      },
    )

    it(
      "`applyChanges` should result in correct intervals with changes",
      () => {
        assert_(
          property1(
            Change.arbitraryBatch(),
            changes => {
              // Js.log("\nchanges:    " ++ changes->Array.map(Change.toString)->Util.Pretty.array)
              let result = Intervals.empty->Intervals.applyChanges(changes)
              // Js.log("intervals:  " ++ result->Intervals.toString)
              Intervals.debugIsValid(result)
              result->Intervals.hasError == None && result->Intervals.isValidWRTChanges(changes)
            },
          ),
        )
      },
    )

    it(
      "`applyChanges` twice should result in correct intervals with changes",
      () => {
        assert_(
          property2(
            Change.arbitraryBatch(),
            Change.arbitraryBatch(),
            (batch1, batch2) => {
              let batches = [batch1, batch2]

              let intervals =
                Intervals.empty->Intervals.applyChanges(batch1)->Intervals.applyChanges(batch2)
              Assert.deepStrictEqual(intervals->Intervals.hasError, None)
              intervals->Intervals.isValidWRTChangeBatches(batches)
            },
          ),
        )
      },
    )
  })
})
