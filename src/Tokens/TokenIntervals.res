// Interval tracking for document changes
// Extracted from Tokens.res for better modularity

// For example: if we replace the text
//    1. between [12-16) with a 6-character-long string
//    2. between [20-24) with a 3-character-long string
// The resulting documents would be represented as:
//
//   Replace(12, 16, 2, Replace(20, 24, 1, EOF))
//
//   12┣━━┫16 +2 20┣━━┫24 +1
//
//  The second delta is 1 instead of -1 because it has to account for the previous delta of 2
type rec t =
  | EOF
  | Replace(int, int, int, t) // start of replacement, end of replacement, delta so far, tail

let deltaToString = delta =>
  if delta > 0 {
    " +" ++ string_of_int(delta) ++ " "
  } else if delta < 0 {
    " " ++ string_of_int(delta) ++ " "
  } else {
    " +0 "
  }

let rec toString = xs =>
  switch xs {
  | EOF => "EOF"
  | Replace(start, end, delta, tail) =>
    if start == end {
      string_of_int(start) ++
      "┃" ++
      string_of_int(end) ++
      deltaToString(delta) ++ if tail == EOF {
        ""
      } else {
        toString(tail)
      }
    } else {
      string_of_int(start) ++
      "┣━━┫" ++
      string_of_int(end) ++
      deltaToString(delta) ++ if tail == EOF {
        ""
      } else {
        toString(tail)
      }
    }
  }

// An empty list of intervals is just EOF
let empty = EOF

// We consider the intervals to be valid if:
//    1. The intervals are non-overlapping
//    2. The intervals are in ascending order
//    3. Inserted length >= 0
//    4. The intervals are not empty
type error =
  | Overlapping // intervals are overlapping
  | ReversedOrder // intervals are in reversed order
  | NegativeInsertion // insertion length < 0
  | Empty

let hasError = xs => {
  let rec aux = (prevEnd, before, xs) => {
    switch xs {
    | EOF => None
    | Replace(start, end, after, tail) =>
      let removed = end - start
      let inserted = after - before + removed

      if start < prevEnd {
        Some(Overlapping) // overlapping intervals
      } else if start > end {
        Some(ReversedOrder) // reversed order
      } else if inserted < 0 {
        Some(NegativeInsertion)
      } else if start == end && inserted == 0 {
        Some(Empty)
      } else {
        aux(end, after, tail)
      }
    }
  }
  aux(0, 0, xs)
}

// print out debug information if `hasError` is not `None`
let debugIsValid = xs => {
  let aux = switch hasError(xs) {
  | None => None
  | Some(Overlapping) => Some("Has overlapping intervals")
  | Some(ReversedOrder) => Some("Has reversed order intervals")
  | Some(NegativeInsertion) => Some("Has negative insertion")
  | Some(Empty) => Some("Has empty intervals")
  }
  switch aux {
  | None => ()
  | Some(error) =>
    Js.log("Intervals: " ++ toString(xs))
    Js.log("Error: " ++ error)
  }
}

// For testing: the last delta should be the total delta
let rec totalDelta = xs =>
  switch xs {
  | EOF => 0
  | Replace(_, _, delta, EOF) => delta
  | Replace(_, _, _, tail) => totalDelta(tail)
  }

// For testing: returns an array of removed intervals
let rec removedIntervals = xs =>
  switch xs {
  | EOF => []
  | Replace(start, end, _, tail) =>
    if start == end {
      removedIntervals(tail)
    } else {
      [(start, end), ...removedIntervals(tail)]
    }
  }

// Intervals are valid wrt changes if:
//    1. The intervals are valid
//    2. The intervals have the same total delta as the changes
//    3. The intervals have the same removed intervals as the changes
let isValidWRTChanges = (xs, changes) => {
  let sameTotalDelta = totalDelta(xs) == TokenChange.totalDelta(changes)
  let sameRemovedIntervals =
    removedIntervals(xs) ==
      changes->Array.map(TokenChange.removedInterval)->Array.filterMap(x => x)
  hasError(xs) == None && sameTotalDelta && sameRemovedIntervals
}

// Intervals are valid wrt batches of changes if:
//    1. The intervals are valid
//    2. The intervals have the same total delta as the batches of changes
let isValidWRTChangeBatches = (xs, batches) => {
  let sameTotalDelta =
    totalDelta(xs) ==
      batches->Array.reduce(0, (acc, changes) => acc + TokenChange.totalDelta(changes))
  // let sameRemovedIntervals =
  //   removedIntervals(xs) == changes->Array.map(TokenChange.removedInterval)->Array.filterMap(x => x)
  hasError(xs) == None && sameTotalDelta
}

//    Suppose a batch of changes has been applied to a document,
//    resulting in an Interval like this in the middle of the document:
//
//        +before ┣━━━━━━━━━━━┫ +after
//                start       end
//
//    Now have a new document with the text between
//        [...  , start) translated to [... , start+before)
//        [start,   end) removed and replaced with some new text
//        [end  ,   ...) translated to [end+after, ...)
//
//        ━━━━━━original━━━━━━┫    replacement  ┣━━━━━original━━━━━
//                            start+before      end+after
//
//    If later a new change is applied to the document,
//    we'll need to calculate where it actually lands in terms of the original document.
//    Offsets between
//        [...         , start+before) should be mapped back to [... , start)
//        [start+before,    end+after) do not exist in the original document
//        [end+after   ,          ...) should be mapped back to [end , ...)
//
//
module Source = {
  type t =
    | Before(int) // exists in the original document at this offset before the existing insertion
    | InInsertion(int)
    | After(int) // exists in the original document at this offset after the existing insertion

  let toString = x =>
    switch x {
    | Before(x) => "Before " ++ string_of_int(x)
    | InInsertion(x) => "InInsertion " ++ string_of_int(x)
    | After(x) => "After " ++ string_of_int(x)
    }

  let calculateOriginalOffset = (deltaBefore, start, end, deltaAfter, x) =>
    if x >= end + deltaAfter {
      After(x - deltaAfter)
    } else if x <= deltaBefore + start {
      Before(x - deltaBefore)
    } else {
      InInsertion(x - deltaBefore)
    }
}

let printInterval = (deltaBefore, start, end, deltaAfter) => {
  Js.log("interval delta before: " ++ string_of_int(deltaBefore))
  Js.log("         delta after: " ++ string_of_int(deltaAfter))
  Js.log("         start: " ++ string_of_int(start))
  Js.log("         end: " ++ string_of_int(end))
  Js.log("         removed: " ++ string_of_int(end - start))
  Js.log("         inserted: " ++ string_of_int(deltaAfter - deltaBefore + end - start))
}

// helper function for adding an interval to the list of intervals
// if the start and end are the same and it inserts nothing, we don't need to add it
let addInterval = (deltaBefore, start, end, deltaAfter, xs) =>
  if start == end && deltaBefore == deltaAfter {
    xs
  } else {
    Replace(start, end, deltaAfter, xs)
  }

// xs: intervals
// changes: list of incoming changes
// deltaBefore: for calculating insertion of intervals
let rec applyChangeAux = (
  xs: t,
  deltaBefore: int,
  translation: int,
  changes: list<TokenChange.t>,
) =>
  switch changes {
  | list{} =>
    switch xs {
    | EOF => EOF
    | Replace(start, end, deltaAfter, tail) =>
      let deltaAfter = deltaAfter + translation
      addInterval(
        deltaBefore,
        start,
        end,
        deltaAfter,
        applyChangeAux(tail, deltaAfter, translation, list{}),
      )
    }
  | list{change, ...changes} =>
    switch xs {
    | EOF =>
      let deltaAfter = deltaBefore + TokenChange.delta(change)

      addInterval(
        deltaBefore,
        change.offset - deltaBefore,
        change.offset + change.removed - deltaBefore,
        deltaAfter,
        applyChangeAux(EOF, deltaAfter, translation, changes),
      )
    | Replace(start, end, deltaAfter, tail) =>
      let deltaAfter = deltaAfter + translation
      let changeRemovalStart = Source.calculateOriginalOffset(
        deltaBefore,
        start,
        end,
        deltaAfter,
        change.offset,
      )
      let changeRemovalEnd = Source.calculateOriginalOffset(
        deltaBefore,
        start,
        end,
        deltaAfter,
        change.offset + change.removed,
      )
      switch (changeRemovalStart, changeRemovalEnd) {
      | (_, InInsertion(changeEnd)) =>
        if end >= changeEnd {
          //
          //          +before ┣━━━━━━┫━━━━━━━┫
          //                  start          end
          //
          //          +before ┣━━━━━━┫━━━┫ +before + interval.insertion - interval.removal
          //                  start      end
          //
          //                  ━━━━━━━┫
          //                         changeEnd
          //
          // split the interval into 2 parts:
          //   1. [start, changeEnd) with removal and insertion (delta = 0)
          //   2. [changeEnd, end) with removal and insertion
          applyChangeAux(
            Replace(
              start,
              changeEnd,
              deltaBefore - translation,
              Replace(changeEnd, end, deltaAfter - translation, tail),
            ),
            deltaBefore,
            translation,
            list{change, ...changes},
          )
        } else {
          //          +before ┣━━━━━┫
          //                  start end
          //
          //          +before ┣━━━━━━┫━━━┫ +before + interval.insertion - interval.removal
          //                  start
          //
          //                  ━━━━━━━┫
          //                         changeEnd
          //
          // split the interval into 2 parts:
          //   1. [start, end) with removal and insertion
          //   2. [end, end) with insertion only

          let part1Delta = changeEnd - end
          applyChangeAux(
            Replace(
              start,
              end,
              deltaBefore + part1Delta - translation,
              Replace(end, end, deltaAfter - translation, tail),
            ),
            deltaBefore,
            translation,
            list{change, ...changes},
          )
        }
      | (Before(changeStart), Before(changeEnd)) =>
        // the whole removal of the change is before the insertion of the interval
        //
        //              +before ┣━━━━━━━━┫ +after
        //                      start    end
        //
        //    ┣━━━━━━━━━━━━━┫
        //    changeStart   changeEnd
        //
        let delta = TokenChange.delta(change)

        addInterval(
          deltaBefore,
          changeStart,
          changeEnd,
          deltaBefore + delta,
          applyChangeAux(xs, deltaBefore + delta, translation + delta, changes),
        )
      | (Before(changeStart), After(_)) =>
        // the whole removal of the change is contains the insertion of the interval
        //
        //              +before ┣━━━━━━━━━━━━━┫ +after
        //                      start         end
        //
        //              ┣━━━━━━━  removal1  ━━┫━━ removal2 ━━┫
        //              changeStart           end            changeEnd
        //
        // we break the change into 2 parts, extend the interval with the first part, and then apply the second part after

        let delta = changeStart - end + deltaBefore - deltaAfter

        let change' = {
          TokenChange.offset: end + deltaAfter + delta,
          removed: change.removed + delta,
          inserted: change.inserted,
        }

        addInterval(
          deltaBefore,
          changeStart,
          end,
          deltaAfter + delta,
          applyChangeAux(
            tail,
            deltaAfter + delta,
            translation + delta,
            list{change', ...changes},
          ),
        )

      | (InInsertion(_), After(changeEnd)) =>
        // the front of the removal of the change overlaps with the back of the insertion of the interval
        //
        //   +before ┣━━━━━━━━━━━━━━━━━━━━━━┫ +after
        //           start                  end
        //
        //                   ┣━━ removal1 ━━┫━━ removal2 ━━┫
        //                   changeStart    end            changeEnd
        //
        let delta = changeEnd - end - change.removed

        let change' = {
          TokenChange.offset: end + deltaAfter + delta,
          removed: change.removed + delta,
          inserted: change.inserted,
        }

        addInterval(
          deltaBefore,
          start,
          end,
          deltaAfter + delta,
          applyChangeAux(
            tail,
            deltaAfter + delta,
            translation + delta,
            list{change', ...changes},
          ),
        )

      | (After(_), After(_)) =>
        // the whole removal of the change is after the insertion of the interval
        //
        //    +before ┣━━━━━━━━┫ +after
        //            start    end
        //
        //                          ┣━━━━━━━━━━━━━━━━┫
        //                          changeStart      changeEnd
        //

        addInterval(
          deltaBefore,
          start,
          end,
          deltaAfter,
          applyChangeAux(tail, deltaAfter, translation, list{change, ...changes}),
        )
      | _ =>
        Js.Exn.raiseError(
          "Not a possible case: " ++
          Source.toString(changeRemovalStart) ++
          " " ++
          Source.toString(changeRemovalEnd),
        )
      }
    }
  }

// TODO: see if we can merge this with the above function
let preprocessChangeBatch = (changes: array<TokenChange.t>) =>
  changes
  ->Array.reduce((0, []), ((delta, acc), x) => {
    let acc = [...acc, x->TokenChange.translate(delta)]
    (delta + TokenChange.delta(x), acc)
  })
  ->snd

// NOTE: the incoming changes should be in ascending order
let applyChanges = (xs: t, changes: array<TokenChange.t>) => {
  applyChangeAux(xs, 0, 0, List.fromArray(changes->preprocessChangeBatch))
}