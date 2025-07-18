module Aspect = Highlighting__AgdaAspect

// phantom types for differentiating offsets counted by Agda and VSCode
type agdaOffset
type vscodeOffset

// information of Tokens from Agda
module Token = {
  open Parser.SExpression
  type filepath = string
  type t<'a> = {
    start: int, // agda offset
    end: int, // agda offset
    aspects: array<Aspect.t>, // a list of names of aspects
    isTokenBased: bool,
    note: option<string>,
    source: option<(Parser.Filepath.t, int)>, // The defining module and the position in that module
  }
  let toStringWithoutOffsets = self =>
    self.aspects->Util.Pretty.array(Aspect.toString) ++
      switch self.source {
      | None => ""
      | Some((_s, i)) => " [src: " ++ string_of_int(i) ++ "]"
      }
  let toString = self =>
    "(" ++
    string_of_int(self.start) ++
    ", " ++
    string_of_int(self.end) ++
    ") " ++
    toStringWithoutOffsets(self)

  // from SExpression
  let parse: Parser.SExpression.t => option<t<agdaOffset>> = x =>
    switch x {
    | A(_) => None
    | L(xs) =>
      switch xs {
      | [A(start'), A(end'), aspects, _, _, L([A(filepath), _, A(index')])] =>
        int_of_string_opt(start')->Option.flatMap(start =>
          int_of_string_opt(end')->Option.flatMap(end =>
            int_of_string_opt(index')->Option.map(
              index => {
                start: start - 1,
                end: end - 1,
                aspects: flatten(aspects)->Array.map(Aspect.parse),
                isTokenBased: false, // NOTE: fix this
                note: None, // NOTE: fix this
                source: Some((Parser.Filepath.make(filepath), index)),
              },
            )
          )
        )

      | [A(start'), A(end'), aspects] =>
        int_of_string_opt(start')->Option.flatMap(start =>
          int_of_string_opt(end')->Option.map(end => {
            start: start - 1,
            end: end - 1,
            aspects: flatten(aspects)->Array.map(Aspect.parse),
            isTokenBased: false, // NOTE: fix this
            note: None, // NOTE: fix this
            source: None,
          })
        )
      | [A(start'), A(end'), aspects, _] =>
        int_of_string_opt(start')->Option.flatMap(start =>
          int_of_string_opt(end')->Option.map(end => {
            start: start - 1,
            end: end - 1,
            aspects: flatten(aspects)->Array.map(Aspect.parse),
            isTokenBased: false, // NOTE: fix this
            note: None, // NOTE: fix this
            source: None,
          })
        )
      | _ => None
      }
    }

  // from SExpression
  let parseDirectHighlightings: array<Parser.SExpression.t> => array<t<agdaOffset>> = tokens =>
    tokens->Array.sliceToEnd(~start=2)->Array.map(parse)->Array.filterMap(x => x)

  // from JSON
  let decodeToken = {
    open JsonCombinators.Json.Decode
    Util.Decode.tuple6(
      int,
      int,
      array(string),
      bool,
      option(string),
      option(pair(string, int)),
    )->map(((start, end, aspects, isTokenBased, note, source)) => {
      start: start - 1,
      end: end - 1,
      aspects: aspects->Array.map(Aspect.parse),
      isTokenBased,
      note,
      source: source->Option.map(((filepath, offset)) => (Parser.Filepath.make(filepath), offset)),
    })
  }

  // from JSON
  let decodeResponseHighlightingInfoDirect = {
    open JsonCombinators.Json.Decode
    pair(bool, array(decodeToken))->map(((keepHighlighting, xs)) => (keepHighlighting, xs))
  }
}

module Change = {
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
}

module Intervals = {
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
    let sameTotalDelta = totalDelta(xs) == Change.totalDelta(changes)
    let sameRemovedIntervals =
      removedIntervals(xs) == changes->Array.map(Change.removedInterval)->Array.filterMap(x => x)
    hasError(xs) == None && sameTotalDelta && sameRemovedIntervals
  }

  // Intervals are valid wrt batches of changes if:
  //    1. The intervals are valid
  //    2. The intervals have the same total delta as the batches of changes
  let isValidWRTChangeBatches = (xs, batches) => {
    let sameTotalDelta =
      totalDelta(xs) == batches->Array.reduce(0, (acc, changes) => acc + Change.totalDelta(changes))
    // let sameRemovedIntervals =
    //   removedIntervals(xs) == changes->Array.map(Change.removedInterval)->Array.filterMap(x => x)
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
  let rec applyChangeAux = (xs: t, deltaBefore: int, translation: int, changes: list<Change.t>) =>
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
        let deltaAfter = deltaBefore + Change.delta(change)

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
          let delta = Change.delta(change)

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
            Change.offset: end + deltaAfter + delta,
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
            Change.offset: end + deltaAfter + delta,
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
  let preprocessChangeBatch = (changes: array<Change.t>) =>
    changes
    ->Array.reduce((0, []), ((delta, acc), x) => {
      let acc = [...acc, x->Change.translate(delta)]
      (delta + Change.delta(x), acc)
    })
    ->snd

  // NOTE: the incoming changes should be in ascending order
  let applyChanges = (xs: t, changes: array<Change.t>) => {
    applyChangeAux(xs, 0, 0, List.fromArray(changes->preprocessChangeBatch))
  }
}

module type Module = {
  type t
  let toString: t => string

  let make: option<Resource.t<array<Highlighting__SemanticToken.t>>> => t

  let toTokenArray: t => array<Token.t<vscodeOffset>>
  let toDecorations: t => Map.t<Editor.Decoration.t, array<VSCode.Range.t>>

  // Receive tokens from Agda
  let addEmacsFilePath: (t, string) => unit
  let addJSONFilePath: (t, string) => unit
  let readTempFiles: (t, VSCode.TextEditor.t) => promise<unit>
  let insertWithVSCodeOffsets: (t, Token.t<vscodeOffset>) => unit
  let insertTokens: (t, VSCode.TextEditor.t, array<Token.t<agdaOffset>>) => unit

  // Remove everything
  let reset: t => unit

  // definition provider for go-to-definition
  let goToDefinition: (
    t,
    VSCode.TextDocument.t,
  ) => (
    Parser.Filepath.t,
    VSCode.Position.t,
  ) => option<promise<array<(VSCode.Range.t, string, VSCode.Position.t)>>>

  // Generate highlighting from the deltas and agdaTokens
  let generateHighlighting: (t, VSCode.TextEditor.t) => unit
  let applyEdit: (t, VSCode.TextEditor.t, VSCode.TextDocumentChangeEvent.t) => unit
  let removeDecorations: (t, VSCode.TextEditor.t) => unit
  let applyDecorations: (t, VSCode.TextEditor.t) => unit

  let toOriginalOffset: (t, int) => option<int>

  let getVSCodeTokens: t => Resource.t<array<Highlighting__SemanticToken.t>>
  let getHolePositionsFromLoad: t => Resource.t<Map.t<int, int>>
  let getHoles: t => Map.t<int, Token.t<vscodeOffset>>
  let getHolesSorted: t => array<Token.t<vscodeOffset>>
}

module Module: Module = {
  // decode Response from Agda or from temp files
  module TempFile = {
    type t =
      | Emacs(string)
      | JSON(string)

    let toFilepath = format =>
      switch format {
      | Emacs(filepath) => filepath
      | JSON(filepath) => filepath
      }

    let readAndParse = async format => {
      try {
        let filepath = toFilepath(format)
        let uri = VSCode.Uri.file(filepath)
        let readResult = await FS.readFile(uri)
        let content = switch readResult {
        | Ok(uint8Array) =>
          // Convert Uint8Array to string using TextDecoder
          let decoder = %raw(`new TextDecoder()`)
          decoder->%raw(`function(decoder, arr) { return decoder.decode(arr) }`)(uint8Array)
        | Error(error) => Js.Exn.raiseError("Failed to read file: " ++ error)
        }
        switch format {
        | Emacs(_) =>
          let tokens = switch Parser.SExpression.parse(content)[0] {
          | Some(Ok(L(xs))) => xs
          | _ => []
          }
          // RemoveTokenBasedHighlighting
          let removeTokenBasedHighlighting = switch tokens[0] {
          | Some(A("remove")) => true
          | _ => false
          }
          let tokens = tokens->Array.sliceToEnd(~start=1)->Array.filterMap(Token.parse)
          (removeTokenBasedHighlighting, tokens)
        | JSON(_) =>
          let raw = content
          switch JSON.parseExn(raw) {
          | exception _e => (false, [])
          | json =>
            switch JsonCombinators.Json.decode(json, Token.decodeResponseHighlightingInfoDirect) {
            | Ok((keepHighlighting, tokens)) => (keepHighlighting, tokens)
            | Error(_err) =>
              Js.log("Error in decoding JSON: " ++ _err)
              (false, [])
            }
          }
        }
      } catch {
      | _ => (false, [])
      }
    }
  }

  type t = {
    // from addEmacsFilePath/addJSONFilePath
    mutable tempFiles: array<TempFile.t>,
    // Tokens from Agda, indexed by the starting offset
    mutable agdaTokens: AVLTree.t<Token.t<vscodeOffset>>,
    // Keep track of edits to the document
    mutable deltas: Intervals.t,
    // Tokens with highlighting information and stuff for VSCode, generated from agdaTokens + deltas
    // expected to be updated along with the deltas
    mutable vscodeTokens: Resource.t<array<Highlighting__SemanticToken.t>>,
    mutable decorations: Map.t<Editor.Decoration.t, array<VSCode.Range.t>>,
    // ranges of holes
    mutable holes: Map.t<int, Token.t<vscodeOffset>>,
    mutable holePositions: Resource.t<Map.t<int, int>>,
  }

  let toString = self => {
    let tempFiles = if self.tempFiles->Array.length == 0 {
      ""
    } else {
      "\n    " ++ self.tempFiles->Array.map(TempFile.toFilepath)->Array.join("\n    ")
    }

    let tokens =
      self.agdaTokens
      ->AVLTree.toArray
      ->Array.map(token =>
        Token.toString(token) ++ " " ++ Int.toString(token.start) ++ "-" ++ Int.toString(token.end)
      )
      ->Array.join("\n    ")
    "Tokens:\n  tempFiles (" ++
    string_of_int(self.tempFiles->Array.length) ++
    ") " ++
    tempFiles ++
    "\n  tokens:\n    " ++
    tokens
  }

  let make = vscodeTokensResource => {
    tempFiles: [],
    agdaTokens: AVLTree.make(),
    deltas: Intervals.empty,
    vscodeTokens: switch vscodeTokensResource {
    | None => Resource.make()
    | Some(resource) => resource
    },
    decorations: Map.make(),
    holes: Map.make(),
    holePositions: Resource.make(),
  }

  let insertWithVSCodeOffsets = (self, token: Token.t<vscodeOffset>) => {
    let existing = self.agdaTokens->AVLTree.find(token.start)
    switch existing {
    | None =>
      self.agdaTokens
      ->AVLTree.insert(token.start, token)
      ->ignore

      // if the token is a Hole, then we need to add it to the holes map
      if token.aspects->Array.some(x => x == Aspect.Hole) {
        self.holes->Map.set(token.start, token)
      }
    | Some(old) =>
      // often the new aspects would look exactly like the old ones
      // don't duplicate them in that case
      let areTheSameTokens =
        old.aspects == token.aspects && old.start == token.start && old.end == token.end

      if !areTheSameTokens {
        // TODO: reexamine if we need to merge the aspects or not
        // merge Aspects only when they are different (TODO: should be sets)
        let newAspects =
          old.aspects == token.aspects ? old.aspects : Array.concat(old.aspects, token.aspects)

        let new = {
          ...old,
          aspects: newAspects,
        }

        // merge Aspects
        self.agdaTokens->AVLTree.remove(token.start)->ignore
        self.agdaTokens->AVLTree.insert(token.start, new)

        if token.aspects->Array.some(x => x == Aspect.Hole) {
          // if the token is a Hole, then we need to add it to the holes map
          self.holes->Map.set(token.start, token)
        }
      }
    }
  }

  // insert a bunch of Tokens
  // merge Aspects with the existing Token that occupies the same Range
  let insertTokens = (self, editor, tokens: array<Token.t<agdaOffset>>) => {
    let document = editor->VSCode.TextEditor.document
    let text = Editor.Text.getAll(document)
    let offsetConverter = Agda.OffsetConverter.make(text)
    tokens->Array.forEach(token => {
      let start = Agda.OffsetConverter.convert(offsetConverter, token.start)
      let end = Agda.OffsetConverter.convert(offsetConverter, token.end)
      insertWithVSCodeOffsets(
        self,
        {
          ...token,
          start,
          end,
        },
      )
    })
  }

  let addEmacsFilePath = (self, filepath) => self.tempFiles->Array.push(TempFile.Emacs(filepath))
  let addJSONFilePath = (self, filepath) => self.tempFiles->Array.push(TempFile.JSON(filepath))

  // read temp files and add Tokens added from "addEmacsFilePath" or "addJSONFilePath"
  let readTempFiles = async (self, editor) => {
    // read and parse and concat them
    let xs = await self.tempFiles->Array.map(TempFile.readAndParse)->Promise.all
    let tokens = xs->Array.map(snd)->Array.flat
    insertTokens(self, editor, tokens)
    self.tempFiles = []
  }

  let reset = self => {
    // delete all unhandled temp files
    self.tempFiles->Array.forEach(format => {
      let filepath = TempFile.toFilepath(format)
      let uri = VSCode.Uri.file(filepath)
      // Fire-and-forget: start deletion but don't wait for completion
      let _ = FS.delete(uri)
    })

    // reset the AgdaTokens
    self.agdaTokens = AVLTree.make()

    // reset the deltas
    self.deltas = Intervals.empty

    // reset the vscodeTokens (only when it has ever been set)
    if self.vscodeTokens->Resource.isPending {
      ()
    } else {
      self.vscodeTokens->Resource.set([])
    }
  }

  let toTokenArray = self => self.agdaTokens->AVLTree.toArray
  let toDecorations = self => self.decorations

  // for goto definition
  let lookupSrcLoc = (self, document, offset): option<
    promise<array<(VSCode.Range.t, string, VSCode.Position.t)>>,
  > => {
    self.agdaTokens
    ->AVLTree.lowerBound(offset)
    ->Option.flatMap(token => {
      let srcRange = VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, token.start),
        VSCode.TextDocument.positionAt(document, token.end),
      )
      token.source->Option.map(((filepath, offset)) => (
        srcRange,
        Parser.Filepath.toString(filepath),
        offset,
      ))
    })
    ->Option.map(((srcRange, filepath, offset)) => {
      VSCode.Workspace.openTextDocumentWithFileName(filepath)->Promise.thenResolve(document => {
        let text = Editor.Text.getAll(document)
        let offsetConverter = Agda.OffsetConverter.make(text)
        let offset = Agda.OffsetConverter.convert(offsetConverter, offset - 1)
        let position = VSCode.TextDocument.positionAt(document, offset)
        [(srcRange, filepath, position)]
      })
    })
  }

  // definition provider for go-to-definition
  let goToDefinition = (self, document) => (fileName, position) => {
    // only provide source location, when the filename matched
    let currentFileName = document->VSCode.TextDocument.fileName->Parser.Filepath.make
    let normalizedFileName = fileName
    let offset = VSCode.TextDocument.offsetAt(document, position)

    if normalizedFileName == currentFileName {
      self->lookupSrcLoc(document, offset)
    } else {
      None
    }
  }

  // remove decorations from the editor
  let removeDecorations = (self, editor) => {
    // in order to remove decorations, we need to go through all types of decorations and set them empty
    self.decorations
    ->Map.keys
    ->Iterator.toArray
    ->Array.forEach(decoration => {
      // remove the decoration by applying it with an empty array of ranges
      Editor.Decoration.apply(editor, decoration, [])
    })
  }

  // apply decorations to the editor
  let applyDecorations = (self, editor) => {
    // apply the decorations to the editor
    self.decorations
    ->Map.entries
    ->Iterator.toArray
    ->Array.forEach(((decoration, ranges)) => {
      Editor.Decoration.apply(editor, decoration, ranges)
    })
  }

  // Converts a list of Agda Aspects to a list of VSCode Tokens and decorations
  let convertFromAgdaAspects = xs => {
    let decorations = Map.make()
    let semanticTokens = xs->Array.filterMap(((aspects, range)) => {
      // convert Aspects to TokenType / TokenModifiers / Backgrounds
      let (tokenTypeAndModifiers, emacsDecos) =
        aspects->Array.map(Aspect.toTokenTypeAndModifiersAndDecoration)->Belt.Array.unzip
      let (tokenTypes, tokenModifiers) = tokenTypeAndModifiers->Belt.Array.unzip
      // merge TokenType / TokenModifiers / Backgrounds
      let tokenTypes = tokenTypes->Array.filterMap(x => x)
      let tokenModifiers = tokenModifiers->Array.flat
      emacsDecos->Array.forEach(emacsDeco =>
        switch emacsDeco {
        | None => ()
        | Some(emacsDeco) =>
          let decoration = Highlighting__Decoration.toDecoration(emacsDeco)
          let existingRanges = switch decorations->Map.get(decoration) {
          | None => []
          | Some(ranges) => ranges
          }
          decorations->Map.set(
            decoration,
            [...existingRanges, Highlighting__SemanticToken.SingleLineRange.toVsCodeRange(range)],
          )
        }
      )

      // only 1 TokenType is allowed, so we take the first one
      let semanticToken = tokenTypes[0]->Option.map(tokenType => {
        Highlighting__SemanticToken.range,
        type_: tokenType,
        modifiers: Some(tokenModifiers),
      })
      Some(semanticToken)
    })
    let semanticTokens = semanticTokens->Array.filterMap(x => x)

    (decorations, semanticTokens)
  }

  type action = Remove | Translate(int)

  // Traverse the intervals with a list of tokens with a function to determine what to do with each of them.
  let traverseIntervals = (
    tokens,
    deltas,
    f: ('acc, Token.t<vscodeOffset>, action) => 'acc,
    init: 'acc,
  ) => {
    let rec traverse = (acc, i, intervals, deltaBefore) =>
      switch tokens[i] {
      | None => acc
      | Some(token) =>
        // token WITHIN the rmeoval interval should be removed
        // token AFTER the removal interval should be translated
        switch intervals {
        | Intervals.EOF =>
          //    token    ┣━━━━━━┫
          traverse(f(acc, token, Translate(deltaBefore)), i + 1, intervals, deltaBefore)
        | Replace(removalStart, removeEnd, delta, tail) =>
          if token.end < removalStart {
            //    interval         ┣━━━━━━━━━━━━━━┫
            //    token    ┣━━━━━━┫
            traverse(f(acc, token, Translate(deltaBefore)), i + 1, intervals, deltaBefore)
          } else if token.start < removeEnd {
            //    interval ┣━━━━━━━━━━━━━━┫
            //    token    ┣━━━━━━┫
            // remove this token, move on to the next one
            traverse(f(acc, token, Remove), i + 1, intervals, deltaBefore)
          } else {
            //    interval ┣━━━━━━━━━━━━━━┫ EOF
            //    token                     ┣━━━━━━┫
            // ignore this interval and move on to the next one
            traverse(acc, i, tail, delta)
          }
        }
      }

    traverse(init, 0, deltas, 0)
  }

  // Generate highlighting from the deltas and agdaTokens
  let generateHighlighting = (self, editor) => {
    let document = editor->VSCode.TextEditor.document

    let (aspects, holePositions) = traverseIntervals(
      self->toTokenArray,
      self.deltas,
      ((acc, holePositions), token, action) =>
        switch action {
        | Remove => // remove this token, move on to the next one
          (acc, holePositions)
        | Translate(delta) =>
          let offsetStart = token.start + delta
          let offsetEnd = token.end + delta
          let range = VSCode.Range.make(
            document->VSCode.TextDocument.positionAt(offsetStart),
            document->VSCode.TextDocument.positionAt(offsetEnd),
          )

          // see if the token is a Hole, then we collect it in the holes array
          if token.aspects->Array.some(x => x == Aspect.Hole) {
            holePositions->Map.set(offsetStart, offsetEnd)
          }
          // split the range in case that it spans multiple lines
          let singleLineRanges = Highlighting__SemanticToken.SingleLineRange.splitRange(
            document,
            range,
          )
          let result = singleLineRanges->Array.map(range => (token.Token.aspects, range))
          ([...acc, ...result], holePositions)
        },
      ([], Map.make()),
    )

    let (decorations, semanticTokens) = convertFromAgdaAspects(aspects)

    // provide the highlighting
    self.vscodeTokens->Resource.set(semanticTokens)

    // set the decorations
    removeDecorations(self, editor)
    self.decorations = decorations
    applyDecorations(self, editor)
    // set the holes positions
    self.holePositions->Resource.set(holePositions)
  }

  // Update the deltas and generate new tokens
  let applyEdit = (self, editor, event) => {
    let changes =
      event
      ->VSCode.TextDocumentChangeEvent.contentChanges
      ->Array.map(Change.fromTextDocumentContentChangeEvent)

    // update the deltas
    self.deltas = Intervals.applyChanges(self.deltas, changes->Array.toReversed)
    let _ = generateHighlighting(self, editor)
  }

  // Calculate the original offset from the deltas
  let toOriginalOffset = (self, offset) =>
    traverseIntervals(
      self->toTokenArray,
      self.deltas,
      (acc, token, action) =>
        switch action {
        | Remove => acc // the offset is not within this removal interval, so we ignore it
        | Translate(delta) =>
          if offset >= token.start + delta && offset < token.end + delta {
            // the offset is within this token, so we restore it to the original offset by subtracting the delta
            Some(offset - delta)
          } else {
            // the offset is not within this token, so we ignore it
            acc
          }
        },
      None,
    )

  let getVSCodeTokens = self => self.vscodeTokens
  let getHolePositionsFromLoad = self => self.holePositions
  let getHoles = self => {
    self.holes
  }
  let getHolesSorted = self =>
    self.holes
    ->Map.values
    ->Iterator.toArray
    ->Array.toSorted((x, y) => Int.compare(x.start, y.start))
}

include Module
