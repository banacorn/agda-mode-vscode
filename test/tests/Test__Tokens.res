open Mocha
open Test__Util

open Tokens

describe("Tokens", () => {
  This.timeout(10000)
  describe("GotoDefinition.agda", () => {
    Async.it(
      "should produce 28 tokens",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")
        let tokens =
          ctx.state.tokens
          ->toArray
          ->Array.map(
            ((token, range)) => Editor.Range.toString(range) ++ " " ++ Token.toString(token),
          )
        Assert.deepEqual(Array.length(tokens), 28)
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
            ((token, range)) => Editor.Range.toString(range) ++ " " ++ Token.toString(token),
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

        Assert.deepEqual(
          tokens,
          [
            "0:0-6 (0, 6) [Keyword]",
            "0:7-21 (7, 21) [Module] [src: 1]",
            "0:22-27 (22, 27) [Keyword]",
            "1:0-4 (28, 32) [Keyword]",
            "1:5-6 (33, 34) [Datatype] [src: 34]",
            "1:7-8 (35, 36) [Symbol]",
            "1:9-12 (37, 40) [Primitive] " ++ srcOfPrimitive,
            "1:13-18 (41, 46) [Keyword]",
            "2:2-3 (49, 50) [ConstructorInductive] [src: 50]",
            "2:4-5 (51, 52) [Symbol]",
            "2:6-7 (53, 54) [Datatype] [src: 34]",
            "3:2-3 (57, 58) [ConstructorInductive] [src: 58]",
            "3:4-5 (59, 60) [Symbol]",
            "3:6-7 (61, 62) [Datatype] [src: 34]",
            "3:8-9 (63, 64) [Symbol]",
            "3:10-11 (65, 66) [Datatype] [src: 34]",
            "5:0-3 (68, 71) [Function, Operator] [src: 69]",
            "5:4-5 (72, 73) [Symbol]",
            "5:6-7 (74, 75) [Datatype] [src: 34]",
            "5:8-9 (76, 77) [Symbol]",
            "5:10-11 (78, 79) [Datatype] [src: 34]",
            "5:12-13 (80, 81) [Symbol]",
            "5:14-15 (82, 83) [Datatype] [src: 34]",
            "6:0-1 (84, 85) [Bound] [src: 85]",
            "6:2-3 (86, 87) [Function, Operator] [src: 69]",
            "6:4-5 (88, 89) [Bound] [src: 89]",
            "6:6-7 (90, 91) [Symbol]",
            "6:8-15 (92, 99) [Hole]",
          ],
        )
      },
    )
  })

  // describe_only("Intervals", () => {
  //   open Intervals
  //   let example: t = Head(Moved(0), Cons(12, Removed, Cons(16, Moved(4), Nil)))
  //   // FastCheck.Arbitrary.
  //   toString(example)->Js.log
  // })

  describe_only("Change", () => {
    open FastCheck
    open Property.Sync

    let nonOverlapping = (xs: array<Change.t>) =>
      xs->Array.reduceWithIndex(
        true,
        (acc, _, i) =>
          switch (xs[i], xs[i + 1]) {
          | (Some(a), Some(b)) =>
            let aEnd = a.offset + a.inserted - a.removed
            let bStart = b.offset
            aEnd <= bStart && acc
          | _ => acc
          },
      )

    it(
      "`arbitraryBatch` should generate non-overlapping changes",
      () => {
        assert_(property1(Change.arbitraryBatch(), xs => nonOverlapping(xs)))
      },
    )
  })

  describe_only("Intervals", () => {
    open FastCheck
    open Property.Sync
    // open Intervals
    //
    it(
      "`empty` should should be valid",
      () => {
        Assert.ok(Intervals.empty->Intervals.isValid)
      },
    )

    // it(
    //   "`applyChange` should result in correct delta",
    //   () => {
    //     assert_(
    //       property1(
    //         Change.arbitrary(0),
    //         change => {
    //           // Js.log("change:     " ++ Change.toString(change))
    //           // Js.log("intervals:  " ++ Intervals.toString(Intervals.empty->applyChange(change)))
    //           // Js.log("change d    " ++ change->Change.delta->Int.toString)
    //           // Js.log("intervals d " ++ Intervals.empty->applyChange(change)->Intervals.totalDelta->Int.toString)
    //           Intervals.empty->applyChange(change)->Intervals.totalDelta == Change.delta(change)
    //         },
    //       ),
    //     )
    //   },
    // )

    it(
      "`applyChanges` should result in correct intervals with 1 change",
      () => {
        assert_(
          property1(
            Change.arbitrary(0),
            change => {
              // Js.log("change:     " ++ Change.toString(change))
              // Js.log("intervals:  " ++ Intervals.toString(Intervals.empty->applyChange(change)))
              // Js.log("change d    " ++ change->Change.delta->Int.toString)
              // Js.log("intervals d " ++ Intervals.empty->applyChange(change)->Intervals.totalDelta->Int.toString)
              let result = Intervals.empty->Intervals.applyChanges([change])
              Intervals.debugIsValid(result)
              result->Intervals.isValid && result->Intervals.isValidWRTChanges([change])

            },
          ),
        )
      },
    )

    // it(
    //   "`removedIntervals` should result in correct array",
    //   () => {
    //     assert_(
    //       property1(
    //         Change.arbitrary(0),
    //         change => {
    //           // Js.log("change:     " ++ Change.toString(change))
    //           // Js.log("intervals:  " ++ Intervals.toString(Intervals.empty->applyChange(change)))
    //           // Js.log(
    //           //   "change d    " ++
    //           //   [change]
    //           //   ->Array.filterMap(Change.removedInterval)
    //           //   ->Array.map(((x, y)) => "[" ++ Int.toString(x) ++ "-" ++ Int.toString(y) ++ "]")
    //           //   ->Util.Pretty.array,
    //           // )
    //           // Js.log(
    //           //   "intervals d " ++
    //           //   Intervals.empty
    //           //   ->applyChange(change)
    //           //   ->Intervals.removedIntervals
    //           //   ->Array.map(((x, y)) => "[" ++ Int.toString(x) ++ "-" ++ Int.toString(y) ++ "]")
    //           //   ->Util.Pretty.array,
    //           // )
    //           Intervals.empty->applyChange(change)->Intervals.removedIntervals ==
    //             [change]->Array.filterMap(Change.removedInterval)
    //         },
    //       ),
    //     )
    //   },
    // )
  })
})
