// Document change tracking logic
// Extracted from Tokens.res for better modularity

type t = {
  offset: int, // offset of where the replacement starts
  removed: int, // length of the removed text
  inserted: int, // length of the inserted text
}

let toString = self =>
  "-" ++
  string_of_int(self.removed) ++
  " +" ++
  string_of_int(self.inserted) ++
  " @ " ++
  string_of_int(self.offset)

let fromTextDocumentContentChangeEvent = event => {
  offset: event->VSCode.TextDocumentContentChangeEvent.rangeOffset,
  removed: event->VSCode.TextDocumentContentChangeEvent.rangeLength,
  inserted: event->VSCode.TextDocumentContentChangeEvent.text->String.length,
}

let delta = x => x.inserted - x.removed
let totalDelta = xs => xs->Array.reduce(0, (acc, x) => acc + delta(x))

let isUseless = x =>
  if x.removed == 0 && x.inserted == 0 {
    true
  } else {
    false
  }

let removedInterval = x =>
  if x.removed == 0 {
    None
  } else {
    Some(x.offset, x.offset + x.removed)
  }

// An array of changes are valid if:
//    1. The changes are non-overlapping
//    2. The changes are in ascending order
let areValid = xs =>
  xs
  ->Array.reduce((true, None), ((acc, prevEnd), x) => {
    let end = x.offset + x.removed
    switch prevEnd {
    | None => // we are at the beginning
      (true, Some(end))
    | Some(prevEnd) =>
      if prevEnd > x.offset {
        (false, Some(end)) // overlapping intervals
      } else {
        (acc, Some(end))
      }
    }
  })
  ->fst

let translate = (x, delta) => {
  offset: x.offset + delta,
  removed: x.removed,
  inserted: x.inserted,
}

open FastCheck.Arbitrary

// The kind of change are we generating
type kind = RemovalOnly | InsertionOnly | Mixed

// Given an offset, generates a Change.t with a random offset after that offset (within offset + 10),
// and random removed and inserted lengths (within 0-10).
let arbitrary = (after, ~kind=Mixed): arbitrary<t> => {
  integerRange(after, after + 10)->Derive.chain(offset => {
    Combinators.tuple2(integerRange(0, offset), integerRange(0, 10))->Derive.map(((
      removed,
      inserted,
    )) =>
      switch kind {
      | RemovalOnly => {
          offset,
          removed,
          inserted: 0,
        }
      | InsertionOnly => {
          offset,
          removed: 0,
          inserted,
        }
      | Mixed => {
          offset,
          removed,
          inserted,
        }
      }
    )
  })
}

// Returns an array of non-overlapping Change.t
let arbitraryBatch = (~batchSize=?, ~kind=Mixed): arbitrary<array<t>> => {
  let rec aux = (after, size) => {
    if size == 0 {
      Combinators.constant([])
    } else {
      Derive.chain(arbitrary(after, ~kind), change => {
        Derive.map(aux(change.offset + change.removed + 1, size - 1), changes => {
          [change, ...changes]
        })
      })
    }
  }
  switch batchSize {
  | None => Derive.chain(integerRange(0, 10), size => aux(0, size))
  | Some(batchSize) => aux(0, batchSize)
  }
}