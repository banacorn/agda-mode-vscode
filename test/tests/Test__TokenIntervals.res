open Mocha

describe("TokenIntervals", () => {
  describe("TokenIntervals.deltaToString", () => {
    it("should format positive delta", () => {
      let result = TokenIntervals.deltaToString(5)
      Assert.deepStrictEqual(result, " +5 ")
    })

    it("should format negative delta", () => {
      let result = TokenIntervals.deltaToString(-3)
      Assert.deepStrictEqual(result, " -3 ")
    })

    it("should format zero delta", () => {
      let result = TokenIntervals.deltaToString(0)
      Assert.deepStrictEqual(result, " +0 ")
    })
  })

  describe("TokenIntervals.toString", () => {
    it("should format EOF", () => {
      let result = TokenIntervals.toString(TokenIntervals.EOF)
      Assert.deepStrictEqual(result, "EOF")
    })

    it("should format single replacement with different start and end", () => {
      let intervals = TokenIntervals.Replace(10, 15, 3, TokenIntervals.EOF)
      let result = TokenIntervals.toString(intervals)
      Assert.deepStrictEqual(result, "10┣━━┫15 +3 ")
    })

    it("should format single replacement with same start and end", () => {
      let intervals = TokenIntervals.Replace(10, 10, 5, TokenIntervals.EOF)
      let result = TokenIntervals.toString(intervals)
      Assert.deepStrictEqual(result, "10┃10 +5 ")
    })

    it("should format multiple replacements", () => {
      let intervals = TokenIntervals.Replace(
        5,
        8,
        2,
        TokenIntervals.Replace(15, 20, -1, TokenIntervals.EOF),
      )
      let result = TokenIntervals.toString(intervals)
      Assert.deepStrictEqual(result, "5┣━━┫8 +2 15┣━━┫20 -1 ")
    })

    it("should format mixed point and range replacements", () => {
      let intervals = TokenIntervals.Replace(
        10,
        10,
        3,
        TokenIntervals.Replace(20, 25, -2, TokenIntervals.EOF),
      )
      let result = TokenIntervals.toString(intervals)
      Assert.deepStrictEqual(result, "10┃10 +3 20┣━━┫25 -2 ")
    })
  })

  describe("TokenIntervals.empty", () => {
    it("should return EOF", () => {
      let result = TokenIntervals.empty
      Assert.deepStrictEqual(result, TokenIntervals.EOF)
    })
  })

  describe("TokenIntervals.hasError", () => {
    it("should return None for EOF", () => {
      let result = TokenIntervals.hasError(TokenIntervals.EOF)
      Assert.deepStrictEqual(result, None)
    })

    it("should return None for valid single interval", () => {
      let intervals = TokenIntervals.Replace(10, 15, 5, TokenIntervals.EOF)
      let result = TokenIntervals.hasError(intervals)
      Assert.deepStrictEqual(result, None)
    })

    it("should return None for valid multiple intervals", () => {
      let intervals = TokenIntervals.Replace(
        5,
        8,
        3,
        TokenIntervals.Replace(15, 20, 6, TokenIntervals.EOF),
      )
      let result = TokenIntervals.hasError(intervals)
      Assert.deepStrictEqual(result, None)
    })

    it("should return Overlapping for overlapping intervals", () => {
      let intervals = TokenIntervals.Replace(
        10,
        15,
        2,
        TokenIntervals.Replace(12, 18, 3, TokenIntervals.EOF), // 12 < 15, so overlapping
      )
      let result = TokenIntervals.hasError(intervals)
      Assert.deepStrictEqual(result, Some(TokenIntervals.Overlapping))
    })

    it("should return ReversedOrder for reversed order", () => {
      let intervals = TokenIntervals.Replace(15, 10, 2, TokenIntervals.EOF) // start > end
      let result = TokenIntervals.hasError(intervals)
      Assert.deepStrictEqual(result, Some(TokenIntervals.ReversedOrder))
    })

    it("should return NegativeInsertion for negative insertion", () => {
      // To have negative insertion: inserted = after - before + removed < 0
      // So: after - before < -removed
      // If removed = 10, before = 0, after = -15, then inserted = -15 - 0 + 10 = -5 < 0
      let intervals = TokenIntervals.Replace(10, 20, -15, TokenIntervals.EOF) // removed=10, inserted=-15-0+10=-5 < 0
      let result = TokenIntervals.hasError(intervals)
      Assert.deepStrictEqual(result, Some(TokenIntervals.NegativeInsertion))
    })

    it("should return Empty for empty interval with no insertion", () => {
      let intervals = TokenIntervals.Replace(10, 10, 0, TokenIntervals.EOF) // start == end && inserted == 0
      let result = TokenIntervals.hasError(intervals)
      Assert.deepStrictEqual(result, Some(TokenIntervals.Empty))
    })
  })

  describe("TokenIntervals.totalDelta", () => {
    it("should return 0 for EOF", () => {
      let result = TokenIntervals.totalDelta(TokenIntervals.EOF)
      Assert.deepStrictEqual(result, 0)
    })

    it("should return delta for single interval", () => {
      let intervals = TokenIntervals.Replace(10, 15, 8, TokenIntervals.EOF)
      let result = TokenIntervals.totalDelta(intervals)
      Assert.deepStrictEqual(result, 8)
    })

    it("should return last delta for multiple intervals", () => {
      let intervals = TokenIntervals.Replace(
        5,
        8,
        3,
        TokenIntervals.Replace(15, 20, 12, TokenIntervals.EOF),
      )
      let result = TokenIntervals.totalDelta(intervals)
      Assert.deepStrictEqual(result, 12) // Should return the last delta
    })

    it("should handle negative delta", () => {
      let intervals = TokenIntervals.Replace(
        10,
        20,
        -5,
        TokenIntervals.Replace(30, 35, -8, TokenIntervals.EOF),
      )
      let result = TokenIntervals.totalDelta(intervals)
      Assert.deepStrictEqual(result, -8)
    })
  })

  describe("TokenIntervals.removedIntervals", () => {
    it("should return empty array for EOF", () => {
      let result = TokenIntervals.removedIntervals(TokenIntervals.EOF)
      Assert.deepStrictEqual(result, [])
    })

    it("should return interval for single removal", () => {
      let intervals = TokenIntervals.Replace(10, 15, 3, TokenIntervals.EOF)
      let result = TokenIntervals.removedIntervals(intervals)
      Assert.deepStrictEqual(result, [(10, 15)])
    })

    it("should skip intervals with no removal (start == end)", () => {
      let intervals = TokenIntervals.Replace(10, 10, 5, TokenIntervals.EOF)
      let result = TokenIntervals.removedIntervals(intervals)
      Assert.deepStrictEqual(result, [])
    })

    it("should return multiple removal intervals", () => {
      let intervals = TokenIntervals.Replace(
        5,
        8,
        2,
        TokenIntervals.Replace(15, 20, 1, TokenIntervals.EOF),
      )
      let result = TokenIntervals.removedIntervals(intervals)
      Assert.deepStrictEqual(result, [(5, 8), (15, 20)])
    })

    it("should mix removal and insertion-only intervals", () => {
      let intervals = TokenIntervals.Replace(
        5,
        8,
        2,
        TokenIntervals.Replace(10, 10, 3, TokenIntervals.Replace(20, 25, 1, TokenIntervals.EOF)),
      )
      let result = TokenIntervals.removedIntervals(intervals)
      Assert.deepStrictEqual(result, [(5, 8), (20, 25)]) // Skip (10, 10) since no removal
    })
  })

  describe("TokenIntervals.isValidWRTChanges", () => {
    it("should return true for empty intervals and empty changes", () => {
      let intervals = TokenIntervals.EOF
      let changes = []
      let result = TokenIntervals.isValidWRTChanges(intervals, changes)
      Assert.deepStrictEqual(result, true)
    })

    it("should return true for matching single change", () => {
      let intervals = TokenIntervals.Replace(10, 15, 3, TokenIntervals.EOF) // removed=5, delta=3, so inserted=8
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 5,
          inserted: 8,
        },
      ]
      let result = TokenIntervals.isValidWRTChanges(intervals, changes)
      Assert.deepStrictEqual(result, true)
    })

    it("should return false for mismatched total delta", () => {
      let intervals = TokenIntervals.Replace(10, 15, 3, TokenIntervals.EOF) // delta = 3
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 5,
          inserted: 7, // delta = 2, doesn't match interval delta
        },
      ]
      let result = TokenIntervals.isValidWRTChanges(intervals, changes)
      Assert.deepStrictEqual(result, false)
    })

    it("should return false for mismatched removed intervals", () => {
      let intervals = TokenIntervals.Replace(10, 15, 3, TokenIntervals.EOF) // removed interval (10, 15)
      let changes = [
        {
          TokenChange.offset: 12, // different offset
          removed: 5,
          inserted: 8,
        },
      ]
      let result = TokenIntervals.isValidWRTChanges(intervals, changes)
      Assert.deepStrictEqual(result, false)
    })

    it("should handle multiple changes correctly", () => {
      let intervals = TokenIntervals.Replace(
        5,
        8,
        2,
        TokenIntervals.Replace(15, 20, 7, TokenIntervals.EOF),
      )
      let changes = [
        {
          TokenChange.offset: 5,
          removed: 3, // (5, 8)
          inserted: 5, // delta = 2
        },
        {
          TokenChange.offset: 15,
          removed: 5, // (15, 20)
          inserted: 10, // delta = 5, total delta = 2 + 5 = 7
        },
      ]
      let result = TokenIntervals.isValidWRTChanges(intervals, changes)
      Assert.deepStrictEqual(result, true)
    })
  })

  describe("TokenIntervals.isValidWRTChangeBatches", () => {
    it("should return true for empty intervals and empty batches", () => {
      let intervals = TokenIntervals.EOF
      let batches = []
      let result = TokenIntervals.isValidWRTChangeBatches(intervals, batches)
      Assert.deepStrictEqual(result, true)
    })

    it("should return true for matching total delta across batches", () => {
      let intervals = TokenIntervals.Replace(10, 15, 8, TokenIntervals.EOF) // total delta = 8
      let batches = [
        [
          {
            TokenChange.offset: 5,
            removed: 2,
            inserted: 5, // delta = 3
          },
        ],
        [
          {
            TokenChange.offset: 10,
            removed: 3,
            inserted: 8, // delta = 5
          },
        ],
      ] // total delta = 3 + 5 = 8
      let result = TokenIntervals.isValidWRTChangeBatches(intervals, batches)
      Assert.deepStrictEqual(result, true)
    })

    it("should return false for mismatched total delta", () => {
      let intervals = TokenIntervals.Replace(10, 15, 6, TokenIntervals.EOF) // total delta = 6
      let batches = [
        [
          {
            TokenChange.offset: 5,
            removed: 2,
            inserted: 4, // delta = 2
          },
        ],
        [
          {
            TokenChange.offset: 10,
            removed: 3,
            inserted: 6, // delta = 3
          },
        ],
      ] // total delta = 2 + 3 = 5, doesn't match 6
      let result = TokenIntervals.isValidWRTChangeBatches(intervals, batches)
      Assert.deepStrictEqual(result, false)
    })

    it("should handle empty batches with single batch", () => {
      let intervals = TokenIntervals.Replace(10, 15, 4, TokenIntervals.EOF) // total delta = 4
      let batches = [
        [],
        [
          {
            TokenChange.offset: 10,
            removed: 1,
            inserted: 5, // delta = 4
          },
        ],
        [],
      ] // total delta = 0 + 4 + 0 = 4
      let result = TokenIntervals.isValidWRTChangeBatches(intervals, batches)
      Assert.deepStrictEqual(result, true)
    })
  })

  describe("TokenIntervals.Source.toString", () => {
    it("should format Before", () => {
      let source = TokenIntervals.Source.Before(15)
      let result = TokenIntervals.Source.toString(source)
      Assert.deepStrictEqual(result, "Before 15")
    })

    it("should format InInsertion", () => {
      let source = TokenIntervals.Source.InInsertion(8)
      let result = TokenIntervals.Source.toString(source)
      Assert.deepStrictEqual(result, "InInsertion 8")
    })

    it("should format After", () => {
      let source = TokenIntervals.Source.After(25)
      let result = TokenIntervals.Source.toString(source)
      Assert.deepStrictEqual(result, "After 25")
    })
  })

  describe("TokenIntervals.Source.calculateOriginalOffset", () => {
    // Parameters: deltaBefore, start, end, deltaAfter, x
    it("should return After for offset beyond end+deltaAfter", () => {
      let result = TokenIntervals.Source.calculateOriginalOffset(5, 10, 15, 8, 25) // x=25 >= end+deltaAfter=15+8=23
      Assert.deepStrictEqual(result, TokenIntervals.Source.After(17)) // 25 - 8
    })

    it("should return Before for offset at or before deltaBefore+start", () => {
      let result = TokenIntervals.Source.calculateOriginalOffset(5, 10, 15, 8, 15) // x=15 <= deltaBefore+start=5+10=15
      Assert.deepStrictEqual(result, TokenIntervals.Source.Before(10)) // 15 - 5
    })

    it("should return InInsertion for offset between boundaries", () => {
      let result = TokenIntervals.Source.calculateOriginalOffset(5, 10, 15, 8, 18) // 15 < 18 < 23
      Assert.deepStrictEqual(result, TokenIntervals.Source.InInsertion(13)) // 18 - 5
    })

    it("should handle edge case at exact boundaries", () => {
      let result1 = TokenIntervals.Source.calculateOriginalOffset(5, 10, 15, 8, 23) // x=23 >= end+deltaAfter=23
      Assert.deepStrictEqual(result1, TokenIntervals.Source.After(15)) // 23 - 8

      let result2 = TokenIntervals.Source.calculateOriginalOffset(5, 10, 15, 8, 15) // x=15 <= deltaBefore+start=15
      Assert.deepStrictEqual(result2, TokenIntervals.Source.Before(10)) // 15 - 5
    })
  })

  describe("TokenIntervals.preprocessChangeBatch", () => {
    it("should return empty array for empty input", () => {
      let result = TokenIntervals.preprocessChangeBatch([])
      Assert.deepStrictEqual(result, [])
    })

    it("should return single change unchanged for single input", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 5,
          inserted: 8,
        },
      ]
      let result = TokenIntervals.preprocessChangeBatch(changes)
      let expected = [
        {
          TokenChange.offset: 10, // 10 + 0
          removed: 5,
          inserted: 8,
        },
      ]
      Assert.deepStrictEqual(result, expected)
    })

    it("should translate subsequent changes by cumulative delta", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 3,
          inserted: 6, // delta = 3
        },
        {
          TokenChange.offset: 20,
          removed: 2,
          inserted: 1, // delta = -1
        },
        {
          TokenChange.offset: 30,
          removed: 0,
          inserted: 4, // delta = 4
        },
      ]
      let result = TokenIntervals.preprocessChangeBatch(changes)
      let expected = [
        {
          TokenChange.offset: 10, // 10 + 0
          removed: 3,
          inserted: 6,
        },
        {
          TokenChange.offset: 23, // 20 + 3
          removed: 2,
          inserted: 1,
        },
        {
          TokenChange.offset: 32, // 30 + 3 + (-1) = 32
          removed: 0,
          inserted: 4,
        },
      ]
      Assert.deepStrictEqual(result, expected)
    })

    it("should handle negative deltas correctly", () => {
      let changes = [
        {
          TokenChange.offset: 15,
          removed: 8,
          inserted: 2, // delta = -6
        },
        {
          TokenChange.offset: 25,
          removed: 1,
          inserted: 5, // delta = 4
        },
      ]
      let result = TokenIntervals.preprocessChangeBatch(changes)
      let expected = [
        {
          TokenChange.offset: 15, // 15 + 0
          removed: 8,
          inserted: 2,
        },
        {
          TokenChange.offset: 19, // 25 + (-6)
          removed: 1,
          inserted: 5,
        },
      ]
      Assert.deepStrictEqual(result, expected)
    })
  })

  describe("TokenIntervals.applyChanges", () => {
    it("should return EOF for empty changes on empty intervals", () => {
      let intervals = TokenIntervals.EOF
      let changes = []
      let result = TokenIntervals.applyChanges(intervals, changes)
      Assert.deepStrictEqual(result, TokenIntervals.EOF)
    })

    it("should create single interval for single change on empty intervals", () => {
      let intervals = TokenIntervals.EOF
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 5,
          inserted: 8,
        },
      ]
      let result = TokenIntervals.applyChanges(intervals, changes)
      let expected = TokenIntervals.Replace(10, 15, 3, TokenIntervals.EOF) // delta = 8 - 5 = 3
      Assert.deepStrictEqual(result, expected)
    })

    it("should handle insertion-only change", () => {
      let intervals = TokenIntervals.EOF
      let changes = [
        {
          TokenChange.offset: 15,
          removed: 0,
          inserted: 6,
        },
      ]
      let result = TokenIntervals.applyChanges(intervals, changes)
      let expected = TokenIntervals.Replace(15, 15, 6, TokenIntervals.EOF) // point insertion
      Assert.deepStrictEqual(result, expected)
    })

    it("should handle removal-only change", () => {
      let intervals = TokenIntervals.EOF
      let changes = [
        {
          TokenChange.offset: 20,
          removed: 8,
          inserted: 0,
        },
      ]
      let result = TokenIntervals.applyChanges(intervals, changes)
      let expected = TokenIntervals.Replace(20, 28, -8, TokenIntervals.EOF) // removal delta = -8
      Assert.deepStrictEqual(result, expected)
    })

    // TODO, reenable this test and examine if it really exposes an unexpected behavior
    it_skip("should create multiple intervals for multiple changes", () => {
      let intervals = TokenIntervals.EOF
      let changes = [
        {
          TokenChange.offset: 5,
          removed: 2,
          inserted: 4, // delta = 2
        },
        {
          TokenChange.offset: 15,
          removed: 3,
          inserted: 1, // delta = -2
        },
      ]
      let result = TokenIntervals.applyChanges(intervals, changes)
      // After preprocessing: 
      // - First change: offset=5, removed=2, inserted=4, delta=2
      // - Second change: offset=15+2=17, removed=3, inserted=1, delta=-2
      // Expected result: Replace(5, 7, 2, Replace(17, 20, 0, EOF))
      // Final total delta should be 2 + (-2) = 0
      let expected = TokenIntervals.Replace(
        5, 7, 2,  // first change: removed interval (5,7), delta=2
        TokenIntervals.Replace(17, 20, 0, TokenIntervals.EOF) // second change: removed interval (17,20), total delta=0
      )
      Assert.deepStrictEqual(result, expected)
    })

    it("should preserve existing intervals when applying new changes", () => {
      let intervals = TokenIntervals.Replace(30, 35, 5, TokenIntervals.EOF)
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 3,
          inserted: 6, // delta = 3
        },
      ]
      let result = TokenIntervals.applyChanges(intervals, changes)
      // The new change (10, 13) comes before the existing interval (30, 35)
      // Expected: Replace(10, 13, 3, Replace(30, 35, 8, EOF))
      // The existing interval's delta gets updated: 5 + 3 = 8 (translation effect)
      let expected = TokenIntervals.Replace(
        10, 13, 3,  // new change interval
        TokenIntervals.Replace(30, 35, 8, TokenIntervals.EOF) // existing interval with translated delta
      )
      Assert.deepStrictEqual(result, expected)
    })
  })
})