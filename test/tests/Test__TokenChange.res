open Mocha

describe("TokenChange", () => {
  describe("TokenChange.toString", () => {
    it("should format change with removal and insertion", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 5,
        inserted: 3,
      }
      let result = TokenChange.toString(change)
      Assert.deepStrictEqual(result, "-5 +3 @ 10")
    })

    it("should format change with only removal", () => {
      let change = {
        TokenChange.offset: 20,
        removed: 8,
        inserted: 0,
      }
      let result = TokenChange.toString(change)
      Assert.deepStrictEqual(result, "-8 +0 @ 20")
    })

    it("should format change with only insertion", () => {
      let change = {
        TokenChange.offset: 15,
        removed: 0,
        inserted: 7,
      }
      let result = TokenChange.toString(change)
      Assert.deepStrictEqual(result, "-0 +7 @ 15")
    })

    it("should format change with no removal or insertion", () => {
      let change = {
        TokenChange.offset: 5,
        removed: 0,
        inserted: 0,
      }
      let result = TokenChange.toString(change)
      Assert.deepStrictEqual(result, "-0 +0 @ 5")
    })
  })

  describe("TokenChange.fromTextDocumentContentChangeEvent", () => {
    it("should convert VSCode text document change event", () => {
      // Mock a VSCode TextDocumentContentChangeEvent
      let mockEvent = %raw(`{
        rangeOffset: 25,
        rangeLength: 10,
        text: "hello world"
      }`)
      
      let result = TokenChange.fromTextDocumentContentChangeEvent(mockEvent)
      let expected = {
        TokenChange.offset: 25,
        removed: 10,
        inserted: 11, // "hello world".length
      }
      Assert.deepStrictEqual(result, expected)
    })

    it("should handle empty text insertion", () => {
      let mockEvent = %raw(`{
        rangeOffset: 0,
        rangeLength: 5,
        text: ""
      }`)
      
      let result = TokenChange.fromTextDocumentContentChangeEvent(mockEvent)
      let expected = {
        TokenChange.offset: 0,
        removed: 5,
        inserted: 0,
      }
      Assert.deepStrictEqual(result, expected)
    })

    it("should handle insertion at position with no removal", () => {
      let mockEvent = %raw(`{
        rangeOffset: 15,
        rangeLength: 0,
        text: "new text"
      }`)
      
      let result = TokenChange.fromTextDocumentContentChangeEvent(mockEvent)
      let expected = {
        TokenChange.offset: 15,
        removed: 0,
        inserted: 8, // "new text".length
      }
      Assert.deepStrictEqual(result, expected)
    })
  })

  describe("TokenChange.delta", () => {
    it("should calculate positive delta for more insertion than removal", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 3,
        inserted: 8,
      }
      let result = TokenChange.delta(change)
      Assert.deepStrictEqual(result, 5) // 8 - 3
    })

    it("should calculate negative delta for more removal than insertion", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 10,
        inserted: 3,
      }
      let result = TokenChange.delta(change)
      Assert.deepStrictEqual(result, -7) // 3 - 10
    })

    it("should calculate zero delta for equal removal and insertion", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 5,
        inserted: 5,
      }
      let result = TokenChange.delta(change)
      Assert.deepStrictEqual(result, 0) // 5 - 5
    })

    it("should calculate delta for only insertion", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 0,
        inserted: 7,
      }
      let result = TokenChange.delta(change)
      Assert.deepStrictEqual(result, 7) // 7 - 0
    })

    it("should calculate delta for only removal", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 6,
        inserted: 0,
      }
      let result = TokenChange.delta(change)
      Assert.deepStrictEqual(result, -6) // 0 - 6
    })
  })

  describe("TokenChange.totalDelta", () => {
    it("should calculate total delta for empty array", () => {
      let result = TokenChange.totalDelta([])
      Assert.deepStrictEqual(result, 0)
    })

    it("should calculate total delta for single change", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 5,
          inserted: 8,
        },
      ]
      let result = TokenChange.totalDelta(changes)
      Assert.deepStrictEqual(result, 3) // (8 - 5)
    })

    it("should calculate total delta for multiple changes", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 5,
          inserted: 8,
        },
        {
          TokenChange.offset: 20,
          removed: 3,
          inserted: 1,
        },
        {
          TokenChange.offset: 30,
          removed: 0,
          inserted: 4,
        },
      ]
      let result = TokenChange.totalDelta(changes)
      Assert.deepStrictEqual(result, 5) // (8-5) + (1-3) + (4-0) = 3 + (-2) + 4 = 5
    })

    it("should handle negative total delta", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 10,
          inserted: 2,
        },
        {
          TokenChange.offset: 30,
          removed: 5,
          inserted: 1,
        },
      ]
      let result = TokenChange.totalDelta(changes)
      Assert.deepStrictEqual(result, -12) // (2-10) + (1-5) = -8 + (-4) = -12
    })
  })

  describe("TokenChange.isUseless", () => {
    it("should return true for change with no removal or insertion", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 0,
        inserted: 0,
      }
      let result = TokenChange.isUseless(change)
      Assert.deepStrictEqual(result, true)
    })

    it("should return false for change with only removal", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 5,
        inserted: 0,
      }
      let result = TokenChange.isUseless(change)
      Assert.deepStrictEqual(result, false)
    })

    it("should return false for change with only insertion", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 0,
        inserted: 3,
      }
      let result = TokenChange.isUseless(change)
      Assert.deepStrictEqual(result, false)
    })

    it("should return false for change with both removal and insertion", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 4,
        inserted: 6,
      }
      let result = TokenChange.isUseless(change)
      Assert.deepStrictEqual(result, false)
    })
  })

  describe("TokenChange.removedInterval", () => {
    it("should return None for change with no removal", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 0,
        inserted: 5,
      }
      let result = TokenChange.removedInterval(change)
      Assert.deepStrictEqual(result, None)
    })

    it("should return interval for change with removal", () => {
      let change = {
        TokenChange.offset: 15,
        removed: 8,
        inserted: 3,
      }
      let result = TokenChange.removedInterval(change)
      Assert.deepStrictEqual(result, Some(15, 23)) // (15, 15 + 8)
    })

    it("should return correct interval for removal at start", () => {
      let change = {
        TokenChange.offset: 0,
        removed: 5,
        inserted: 0,
      }
      let result = TokenChange.removedInterval(change)
      Assert.deepStrictEqual(result, Some(0, 5)) // (0, 0 + 5)
    })

    it("should return correct interval for single character removal", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 1,
        inserted: 2,
      }
      let result = TokenChange.removedInterval(change)
      Assert.deepStrictEqual(result, Some(10, 11)) // (10, 10 + 1)
    })
  })

  describe("TokenChange.areValid", () => {
    it("should return true for empty array", () => {
      let result = TokenChange.areValid([])
      Assert.deepStrictEqual(result, true)
    })

    it("should return true for single change", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 5,
          inserted: 3,
        },
      ]
      let result = TokenChange.areValid(changes)
      Assert.deepStrictEqual(result, true)
    })

    it("should return true for non-overlapping changes in ascending order", () => {
      let changes = [
        {
          TokenChange.offset: 5,
          removed: 3,
          inserted: 2,
        },
        {
          TokenChange.offset: 15,
          removed: 4,
          inserted: 6,
        },
        {
          TokenChange.offset: 25,
          removed: 2,
          inserted: 1,
        },
      ]
      let result = TokenChange.areValid(changes)
      Assert.deepStrictEqual(result, true)
    })

    it("should return true for adjacent non-overlapping changes", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 5,
          inserted: 3,
        },
        {
          TokenChange.offset: 15, // exactly after the first change (10 + 5)
          removed: 2,
          inserted: 4,
        },
      ]
      let result = TokenChange.areValid(changes)
      Assert.deepStrictEqual(result, true)
    })

    it("should return false for overlapping changes", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 8,
          inserted: 3,
        },
        {
          TokenChange.offset: 15, // overlaps with first change (10 + 8 = 18 > 15)
          removed: 2,
          inserted: 4,
        },
      ]
      let result = TokenChange.areValid(changes)
      Assert.deepStrictEqual(result, false)
    })

    it("should return false for changes in wrong order", () => {
      let changes = [
        {
          TokenChange.offset: 20,
          removed: 3,
          inserted: 2,
        },
        {
          TokenChange.offset: 10, // comes after but has smaller offset
          removed: 4,
          inserted: 1,
        },
      ]
      let result = TokenChange.areValid(changes)
      Assert.deepStrictEqual(result, false)
    })

    it("should handle changes with zero removal", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 0,
          inserted: 5,
        },
        {
          TokenChange.offset: 15,
          removed: 3,
          inserted: 2,
        },
      ]
      let result = TokenChange.areValid(changes)
      Assert.deepStrictEqual(result, true)
    })

    it("should handle changes with zero insertion", () => {
      let changes = [
        {
          TokenChange.offset: 10,
          removed: 5,
          inserted: 0,
        },
        {
          TokenChange.offset: 20,
          removed: 3,
          inserted: 7,
        },
      ]
      let result = TokenChange.areValid(changes)
      Assert.deepStrictEqual(result, true)
    })
  })

  describe("TokenChange.translate", () => {
    it("should translate change offset by positive delta", () => {
      let change = {
        TokenChange.offset: 10,
        removed: 5,
        inserted: 3,
      }
      let result = TokenChange.translate(change, 7)
      let expected = {
        TokenChange.offset: 17, // 10 + 7
        removed: 5,
        inserted: 3,
      }
      Assert.deepStrictEqual(result, expected)
    })

    it("should translate change offset by negative delta", () => {
      let change = {
        TokenChange.offset: 20,
        removed: 3,
        inserted: 8,
      }
      let result = TokenChange.translate(change, -5)
      let expected = {
        TokenChange.offset: 15, // 20 - 5
        removed: 3,
        inserted: 8,
      }
      Assert.deepStrictEqual(result, expected)
    })

    it("should translate change offset by zero delta", () => {
      let change = {
        TokenChange.offset: 15,
        removed: 4,
        inserted: 2,
      }
      let result = TokenChange.translate(change, 0)
      let expected = {
        TokenChange.offset: 15, // 15 + 0
        removed: 4,
        inserted: 2,
      }
      Assert.deepStrictEqual(result, expected)
    })

    it("should preserve removed and inserted values", () => {
      let change = {
        TokenChange.offset: 100,
        removed: 25,
        inserted: 40,
      }
      let result = TokenChange.translate(change, 50)
      let expected = {
        TokenChange.offset: 150, // 100 + 50
        removed: 25, // unchanged
        inserted: 40, // unchanged
      }
      Assert.deepStrictEqual(result, expected)
    })
  })

  describe("FastCheck Properties", () => {
    open FastCheck
    open Property.Sync
    
    describe("Composition Associativity", () => {
      it("delta composition should be associative", () => {
        assert_(
          property3(
            TokenChange.arbitrary(0),
            TokenChange.arbitrary(50), 
            TokenChange.arbitrary(100),
            (a, b, c) => {
              // Test associativity of delta accumulation: (a + b) + c == a + (b + c)
              let leftAssoc = TokenChange.delta(a) + TokenChange.delta(b) + TokenChange.delta(c)
              let rightAssoc = TokenChange.delta(a) + (TokenChange.delta(b) + TokenChange.delta(c))
              let totalDelta = TokenChange.totalDelta([a, b, c])
              
              // All three should be equal
              leftAssoc == rightAssoc && leftAssoc == totalDelta
            }
          )
        )
      })
      
      it("batch composition should be associative", () => {
        assert_(
          property3(
            TokenChange.arbitraryBatch(~batchSize=2),
            TokenChange.arbitraryBatch(~batchSize=2),
            TokenChange.arbitraryBatch(~batchSize=2),
            (batch1, batch2, batch3) => {
              // Test that grouping doesn't affect total delta: ((a + b) + c) == (a + (b + c))
              let leftGroup = [
                ...batch1,
                ...batch2
              ]
              let leftTotal = TokenChange.totalDelta(leftGroup) + TokenChange.totalDelta(batch3)
              
              let rightGroup = [
                ...batch2, 
                ...batch3
              ]
              let rightTotal = TokenChange.totalDelta(batch1) + TokenChange.totalDelta(rightGroup)
              
              let allBatches = TokenChange.totalDelta([...batch1, ...batch2, ...batch3])
              
              leftTotal == rightTotal && leftTotal == allBatches
            }
          )
        )
      })
      
      it("translation composition should be associative", () => {
        assert_(
          property4(
            TokenChange.arbitrary(0),
            FastCheck.Arbitrary.integerRange(-50, 50),
            FastCheck.Arbitrary.integerRange(-50, 50), 
            FastCheck.Arbitrary.integerRange(-50, 50),
            (change, delta1, delta2, delta3) => {
              // Test that translate(translate(translate(c, d1), d2), d3) == translate(c, d1 + d2 + d3)
              let stepByStep = change
                ->TokenChange.translate(delta1)
                ->TokenChange.translate(delta2)
                ->TokenChange.translate(delta3)
              
              let allAtOnce = TokenChange.translate(change, delta1 + delta2 + delta3)
              
              stepByStep.offset == allAtOnce.offset &&
              stepByStep.removed == allAtOnce.removed &&
              stepByStep.inserted == allAtOnce.inserted
            }
          )
        )
      })
    })
  })
})