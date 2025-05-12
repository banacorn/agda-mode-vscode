open Mocha
open Test__Util

open Tokens

describe_only("WIP", () => {
  open FastCheck
  open Property.Sync

  let toBatch = tuples =>
    tuples->Array.map(((offset, removed, inserted)) => {Change.offset, removed, inserted})

  let run = (batch1, batch2) => () => {
    let batch1 = batch1->toBatch
    let batch2 = batch2->toBatch

    let batches = [batch1, batch2]

    let intervals = Intervals.empty->Intervals.applyChanges(batch1)
    let intervals = intervals->Intervals.applyChanges(batch2)
    Assert.deepStrictEqual(intervals->Intervals.hasError, None)
    Assert.ok(intervals->Intervals.isValidWRTChangeBatches(batches))
  }

  // it(
  // it_only(
    it_skip(
    "counter example",
    () => {
      // these test cases are to be copied to the regression tests below

      // let batch1 = [(3, 0, 6), (7, 0, 2)]
      // let batch2 = [(10, 4, 0)]

      // let batch1 = [(0, 0, 8), (3, 0, 1)]
      // let batch2 = [(8, 3, 0)]

      let batch1 = [(1, 0, 8), (3, 0, 1)]
      let batch2 = [(8, 3, 0)]

      // let batch1 = [(2, 0, 8), (3, 0, 1)]
      // let batch2 = [(8, 3, 0)]

      // let batch1 = [(0, 0, 5), (10, 0, 1)]
      // let batch2 = [(12, 3, 0)]

      // 0
      // let batch1 = [(10, 5, 3)]
      // let batch2 = [(5, 7, 0)]

      // let batch1 = [(10, 5, 3), (20, 0, 0)]
      // let batch2 = [(5, 7, 0)]

      // [[{"offset":7,"removed":0,"inserted":7},{"offset":18,"removed":16,"inserted":1},{"offset":35,"removed":0,"inserted":0},{"offset":36,"removed":0,"inserted":1}],[{"offset":0,"removed":0,"inserted":0},{"offset":10,"removed":0,"inserted":3},{"offset":16,"removed":12,"inserted":0},{"offset":29,"removed":0,"inserted":0}]]
      // let batch1 = [(7, 0, 7), (18, 16, 1), (36, 0, 1)]
      // let batch2 = [(16, 12, 0)]

      // Counterexample: [[{"offset":7,"removed":2,"inserted":5},{"offset":16,"removed":16,"inserted":8},{"offset":34,"removed":34,"inserted":0},{"offset":74,"removed":1,"inserted":3}],[{"offset":8,"removed":3,"inserted":0},{"offset":19,"removed":18,"inserted":0},{"offset":38,"removed":0,"inserted":0},{"offset":39,"removed":0,"inserted":0},{"offset":40,"removed":0,"inserted":0},{"offset":41,"removed":0,"inserted":0},{"offset":42,"removed":0,"inserted":0},{"offset":43,"removed":0,"inserted":0}]]
      let batch1 = [(20, 39, 0), (74, 1, 3)]
      let batch2 = [(19, 18, 0)]

      // ,[{"offset":9,"removed":2,"inserted":2},{"offset":18,"removed":4,"inserted":5},{"offset":26,"removed":3,"inserted":0},{"offset":38,"removed":27,"inserted":1},{"offset":67,"removed":2,"inserted":0}]]
      let batch1 = [(60, 49, 0), (117, 107, 0), (225, 0, 1)]
      let batch2 = [(38, 28, 1), (67, 2, 0)]

      let batch1 = batch1->toBatch
      let batch2 = batch2->toBatch

      let batches = [batch1, batch2]

      Js.log("\nbatch 1:    " ++ batch1->Array.map(Change.toString)->Util.Pretty.array)
      Js.log("batch 2:    " ++ batch2->Array.map(Change.toString)->Util.Pretty.array)
      let intervals = Intervals.empty->Intervals.applyChanges(batch1)
      Js.log("\n\n\n    " ++ intervals->Intervals.toString)
      let intervals = intervals->Intervals.applyChanges(batch2)
      Js.log("\n => " ++ intervals->Intervals.toString)
      Assert.deepStrictEqual(intervals->Intervals.hasError, None)
      Assert.ok(intervals->Intervals.isValidWRTChangeBatches(batches))
    },
  )

  // here collects all regressions test from the "counter example"
  // the test cases are copied from the "counter example" test above
  it("regression 1", run([(0, 0, 0)], [(0, 0, 0), (0, 0, 1)]))
  it("regression 2", run([(0, 0, 1)], [(1, 0, 1)]))
  it("regression 3", run([(1, 1, 0)], [(0, 0, 0)]))
  it("regression 4", run([(6, 0, 0)], [(4, 3, 0)]))
  it("regression 5", run([(5, 2, 0)], [(6, 0, 0)]))
  it("regression 6", run([(8, 0, 0)], [(8, 5, 0)]))
  it("regression 7", run([(4, 0, 4)], [(7, 2, 0)]))
  it("regression 8", run([(1, 1, 2)], [(2, 0, 0)]))
  it("regression 9", run([(0, 0, 6), (10, 0, 0)], [(9, 1, 0)]))
  it("regression 10", run([(9, 0, 1), (10, 0, 0)], [(6, 4, 0)]))
  it("regression 11", run([(8, 0, 1), (9, 8, 0)], [(6, 4, 0)]))
  it("regression 12", run([(9, 0, 1), (10, 0, 0)], [(9, 2, 0)]))
  it("regression 13", run([(10, 1, 0), (12, 0, 1)], [(5, 5, 0)]))
  it("regression 14", run([(2, 1, 0), (6, 0, 1)], [(4, 2, 0)]))
  it("regression 15", run([(10, 5, 3)], [(5, 15, 0)]))
  it("regression 16", run([(10, 5, 3), (30, 0, 0)], [(5, 15, 0)]))
  it("regression 17", run([(1, 0, 10), (2, 0, 0)], [(7, 6, 0)]))
  it("regression 18", run([(0, 0, 5), (10, 0, 1)], [(12, 3, 0)]))
  it("regression 19", run([(1, 0, 8), (3, 0, 1)], [(8, 3, 0)]))
  it("regression 20", run([(10, 5, 3)], [(5, 7, 0)]))
  it("regression 21", run([(3, 0, 6), (7, 0, 2)], [(10, 4, 0)]))

  it("insertion / insertion", () => {
    assert_(
      property2(
        Change.arbitraryBatch(),
        Change.arbitraryBatch(),
        (batch1, batch2) => {
          let batches = [batch1, batch2]

          // Js.log("\nbatch 1:    " ++ batch1->Array.map(Change.toString)->Util.Pretty.array)
          // Js.log("batch 2:    " ++ batch2->Array.map(Change.toString)->Util.Pretty.array)
          let intervals = Intervals.empty->Intervals.applyChanges(batch1)
          // Js.log("    " ++ intervals->Intervals.toString)
          let intervals = intervals->Intervals.applyChanges(batch2)
          // Js.log("\n => " ++ intervals->Intervals.toString)
          Assert.deepStrictEqual(intervals->Intervals.hasError, None)
          intervals->Intervals.isValidWRTChangeBatches(batches)
        },
      ),
    )
  })
})

// describe_only("WIP", () => {
//   open FastCheck
//   open Property.Sync
//   let genBeforeBefore = (~kind1=Change.Mixed, ~kind2=Change.Mixed) => {
//     open FastCheck.Arbitrary
//     integerRange(0, 10)
//     ->Derive.chain(Change.arbitrary(_, ~kind=kind2))
//     ->Derive.chain(change1 => {
//       let end = change1.offset + change1.removed
//       Change.arbitraryBatch(~kind=kind1)
//       ->Derive.filter(
//         batch => {
//           switch batch[0] {
//           | None => true
//           | Some(change2) =>
//             if change2.offset > end {
//               true
//             } else {
//               false
//             }
//           }
//         },
//       )
//       ->Derive.map(
//         batch => {
//           let batch1 = [change1]
//           let batch2 = batch
//           (batch2, batch1)
//         },
//       )
//     })
//   }

//   let genBeforeReplaced = (~kind1=Change.Mixed, ~kind2=Change.Mixed) => {
//     open FastCheck.Arbitrary
//     integerRange(0, 10)
//     ->Derive.chain(Change.arbitrary(_, ~kind=kind2))
//     ->Derive.chain(change1 => {
//       let start = change1.offset
//       let end = change1.offset + change1.removed
//       Change.arbitraryBatch(~kind=kind1)
//       ->Derive.filter(
//         batch => {
//           switch batch[0] {
//           | None => true
//           | Some(change2) =>
//             let start2 = change2.offset
//             let end2 = change2.offset + change2.removed
//             if start2 > start && start2 <= end && end2 >= end {
//               true
//             } else {
//               false
//             }
//           }
//         },
//       )
//       ->Derive.map(
//         batch => {
//           let batch1 = [change1]
//           let batch2 = batch
//           (batch2, batch1)
//         },
//       )
//     })
//   }

//   let toBatch = tuples =>
//     tuples->Array.map(((offset, removed, inserted)) => {Change.offset, removed, inserted})

//   let run = (batch1, batch2) => () => {
//     let batch1 = [(5, 1, 0), (6, 0, 2)]->toBatch
//     let batch2 = [(4, 2, 0)]->toBatch

//     let batches = [batch1, batch2]

//     let intervals = Intervals.empty->Intervals.applyChanges(batch1)
//     let intervals = intervals->Intervals.applyChanges(batch2)
//     Assert.deepStrictEqual(intervals->Intervals.hasError, None)
//     Assert.ok(intervals->Intervals.isValidWRTChangeBatches(batches))
//   }

//   it("regression 1", run([(5, 1, 0), (6, 0, 2)], [(4, 2, 0)]))
//   it("regression 2", run([(5, 2, 2)], [(4, 3, 0)]))
//   it("regression 3", run([(9, 3, 0), (12, 0, 1)], [(8, 4, 2)]))
//   it("regression 4", run([(7, 0, 1)], [(4, 2, 8)]))
//   it("regression 5 BM", run([(6, 3, 0), (9, 4, 6)], [(5, 4, 0)]))
//   it("regression 6 BM", run([(10, 5, 3)], [(7, 6, 1)]))
//   // it("main", () => {
//   it_skip("main", () => {
//     assert_(
//       property1(
//         // genBeforeBefore(),
//         genBeforeReplaced(),
//         ((batch1, batch2)) => {
//           let batches = [batch1, batch2]

//           Js.log("\nbatch 1:    " ++ batch1->Array.map(Change.toString)->Util.Pretty.array)
//           Js.log("batch 2:    " ++ batch2->Array.map(Change.toString)->Util.Pretty.array)
//           let intervals = Intervals.empty->Intervals.applyChanges(batch1)
//           Js.log("    " ++ intervals->Intervals.toString)
//           let intervals = intervals->Intervals.applyChanges(batch2)
//           Js.log("\n => " ++ intervals->Intervals.toString)
//           Assert.deepStrictEqual(intervals->Intervals.hasError, None)
//           intervals->Intervals.isValidWRTChangeBatches(batches)
//         },
//       ),
//     )
//   })

//   it(
//     // it_only(
//     // it_skip(
//     "counter example",
//     () => {
//       let batch1 = [(9, 1, 0), (12, 0, 0)]
//       let batch2 = [(8, 2, 0)]

//       let batch1 = batch1->toBatch
//       let batch2 = batch2->toBatch

//       let batches = [batch1, batch2]

//       Js.log("\nbatch 1:    " ++ batch1->Array.map(Change.toString)->Util.Pretty.array)
//       Js.log("batch 2:    " ++ batch2->Array.map(Change.toString)->Util.Pretty.array)
//       let intervals = Intervals.empty->Intervals.applyChanges(batch1)
//       Js.log("    " ++ intervals->Intervals.toString)
//       let intervals = intervals->Intervals.applyChanges(batch2)
//       Js.log("\n => " ++ intervals->Intervals.toString)
//       Assert.deepStrictEqual(intervals->Intervals.hasError, None)
//       Assert.ok(intervals->Intervals.isValidWRTChangeBatches(batches))
//     },
//   )
// })

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
            (changes1, changes2) => {
              // Js.log("\nchanges1:    " ++ changes1->Array.map(Change.toString)->Util.Pretty.array)
              // Js.log("changes2:    " ++ changes2->Array.map(Change.toString)->Util.Pretty.array)
              let intervals = Intervals.empty->Intervals.applyChanges(changes1)
              let intervals = intervals->Intervals.applyChanges(changes2)
              // Js.log("intervals:  " ++ intervals->Intervals.toString)
              // Intervals.debugIsValid(intervals)
              intervals->Intervals.hasError == None &&
                intervals->Intervals.isValidWRTChangeBatches([changes1, changes2])
            },
          ),
        )
      },
    )

    // WIP

    // it_only(
    //   // it_skip(
    //   "COUNTEREXAMPLE",
    //   () => {
    //     let batch1 =  [(5, 1, 3),  (6, 10, 0)]->toBatch
    //     let batch2 = [(4, 20, 0)]->toBatch
    //     let batches = [batch1, batch2]

    //     Js.log("\nbatch 1:    " ++ batch1->Array.map(Change.toString)->Util.Pretty.array)
    //     Js.log("batch 2:    " ++ batch2->Array.map(Change.toString)->Util.Pretty.array)
    //     let intervals = Intervals.empty->Intervals.applyChanges(batch1)
    //     Js.log("    " ++ intervals->Intervals.toString)
    //     let intervals = intervals->Intervals.applyChanges(batch2)
    //     Js.log("\n => " ++ intervals->Intervals.toString)
    //     Assert.deepStrictEqual(intervals->Intervals.hasError, None)
    //     Assert.ok(intervals->Intervals.isValidWRTChangeBatches(batches))
    //   },
    // )

    it_skip(
      "COUNTEREXAMPLE: `applyChanges` twice should result in correct intervals with changes",
      () => {
        let example = Intervals.Replace(12, 16, 2, Replace(20, 24, 1, EOF))
        Js.log("example: " ++ Intervals.toString(example))

        let changes1 = [{Change.offset: 0, removed: 0, inserted: 0}]
        let changes2 = [{Change.offset: 0, removed: 0, inserted: 1}]
        let batches = [changes1, changes2]

        let intervals = Intervals.empty->Intervals.applyChanges(changes1)
        let intervals = intervals->Intervals.applyChanges(changes2)

        let ok =
          intervals->Intervals.hasError == None &&
            intervals->Intervals.isValidWRTChangeBatches(batches)

        Js.log("\nchanges1:    " ++ changes1->Array.map(Change.toString)->Util.Pretty.array)
        Js.log("changes2:    " ++ changes2->Array.map(Change.toString)->Util.Pretty.array)
        Js.log("intervals:  " ++ intervals->Intervals.toString)

        Assert.ok(ok)
      },
    )
  })
})
