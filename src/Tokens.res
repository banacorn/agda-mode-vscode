module Aspect = Highlighting__AgdaAspect

// information of Tokens from Agda
module Token = {
  open Parser.SExpression
  type filepath = string
  type t = {
    start: int, // agda offset
    end_: int, // agda offset
    aspects: array<Aspect.t>, // a list of names of aspects
    isTokenBased: bool,
    note: option<string>,
    source: option<(filepath, int)>, // The defining module and the position in that module
  }
  let toString = self =>
    "(" ++
    string_of_int(self.start) ++
    ", " ++
    string_of_int(self.end_) ++
    ") " ++
    Util.Pretty.list(List.fromArray(Array.map(self.aspects, Aspect.toString))) ++
    switch self.source {
    | None => ""
    | Some((_s, i)) => " [src: " ++ string_of_int(i) ++ "]"
    }

  // from SExpression
  let parse: Parser.SExpression.t => option<t> = x =>
    switch x {
    | A(_) => None
    | L(xs) =>
      switch xs {
      | [A(start'), A(end_'), aspects, _, _, L([A(filepath), _, A(index')])] =>
        int_of_string_opt(start')->Option.flatMap(start =>
          int_of_string_opt(end_')->Option.flatMap(end_ =>
            int_of_string_opt(index')->Option.map(
              index => {
                start: start - 1,
                end_: end_ - 1,
                aspects: flatten(aspects)->Array.map(Aspect.parse),
                isTokenBased: false, // NOTE: fix this
                note: None, // NOTE: fix this
                source: Some((filepath, index)),
              },
            )
          )
        )

      | [A(start'), A(end_'), aspects] =>
        int_of_string_opt(start')->Option.flatMap(start =>
          int_of_string_opt(end_')->Option.map(end_ => {
            start: start - 1,
            end_: end_ - 1,
            aspects: flatten(aspects)->Array.map(Aspect.parse),
            isTokenBased: false, // NOTE: fix this
            note: None, // NOTE: fix this
            source: None,
          })
        )
      | [A(start'), A(end_'), aspects, _] =>
        int_of_string_opt(start')->Option.flatMap(start =>
          int_of_string_opt(end_')->Option.map(end_ => {
            start: start - 1,
            end_: end_ - 1,
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
  let parseDirectHighlightings: array<Parser.SExpression.t> => array<t> = tokens =>
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
    )->map(((start, end_, aspects, isTokenBased, note, source)) => {
      start: start - 1,
      end_: end_ - 1,
      aspects: aspects->Array.map(Aspect.parse),
      isTokenBased,
      note,
      source,
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

  let applyChanges = (xs: t, changes: array<Change.t>) => {
    applyChangeAux(xs, 0, 0, List.fromArray(changes->preprocessChangeBatch))
  }
}

module type Module = {
  type t
  let toString: t => string

  let make: option<Resource.t<array<Highlighting__SemanticToken.t>>> => t
  let addEmacsFilePath: (t, string) => unit
  let addJSONFilePath: (t, string) => unit
  let readTempFiles: (t, VSCode.TextEditor.t) => promise<unit>
  let insert: (t, VSCode.TextEditor.t, array<Token.t>) => unit
  let clear: t => unit

  let toArray: t => array<(Token.t, (int, int), VSCode.Range.t)>

  let lookupSrcLoc: (
    t,
    int,
  ) => option<promise<array<(VSCode.Range.t, Token.filepath, VSCode.Position.t)>>>

  let toDecorations: (t, VSCode.TextEditor.t) => array<(Editor.Decoration.t, array<VSCode.Range.t>)>
  let toDecorationsAndSemanticTokens: (
    t,
    VSCode.TextEditor.t,
  ) => (array<(Editor.Decoration.t, array<VSCode.Range.t>)>, array<Highlighting__SemanticToken.t>)

  let applyEdit: (t, VSCode.TextEditor.t, VSCode.TextDocumentChangeEvent.t) => unit

  let getVSCodeTokens: t => Resource.t<array<Highlighting__SemanticToken.t>>
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
        let content = await Node__Fs.readFile(toFilepath(format))
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
    // because AVLTree is crap, we need to save the index (starting offset) alongside the value for easy access
    mutable agdaTokens: AVLTree.t<(Token.t, (int, int), VSCode.Range.t)>,
    // Keep track of edits to the document
    mutable deltas: Intervals.t,
    // Tokens with highlighting information and stuff for VSCode, generated from agdaTokens + deltas
    // expected to be updated along with the deltas
    mutable vscodeTokens: Resource.t<array<Highlighting__SemanticToken.t>>,
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
      ->Array.map(((token, offset, range)) =>
        Token.toString(token) ++ " " ++ Editor.Range.toString(range)
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
  }

  // insert a bunch of Tokens
  // merge Aspects with the existing Token that occupies the same Range
  let insert = (self, editor, tokens: array<Token.t>) => {
    let document = editor->VSCode.TextEditor.document
    let text = Editor.Text.getAll(document)
    let offsetConverter = Agda.OffsetConverter.make(text)
    tokens->Array.forEach(info => {
      let offsetStart = Agda.OffsetConverter.convert(offsetConverter, info.start)
      let offsetEnd = Agda.OffsetConverter.convert(offsetConverter, info.end_)
      let existing = self.agdaTokens->AVLTree.find(offsetStart)
      switch existing {
      | None =>
        let start = VSCode.TextDocument.positionAt(document, offsetStart)

        let end_ = VSCode.TextDocument.positionAt(
          document,
          Agda.OffsetConverter.convert(offsetConverter, info.end_),
        )
        let range = VSCode.Range.make(start, end_)
        self.agdaTokens
        ->AVLTree.insert(offsetStart, (info, (offsetStart, offsetEnd), range))
        ->ignore
      | Some((old, offses, range)) =>
        // merge Aspects
        self.agdaTokens->AVLTree.remove(offsetStart)->ignore
        // often the new aspects would look exactly like the old ones
        // don't duplicate them in that case
        let newAspects =
          old.aspects == info.aspects ? old.aspects : Array.concat(old.aspects, info.aspects)
        let new = {
          ...old,
          aspects: newAspects,
        }
        self.agdaTokens->AVLTree.insert(offsetStart, (new, offses, range))
      }
    })
  }

  let addEmacsFilePath = (self, filepath) => self.tempFiles->Array.push(TempFile.Emacs(filepath))
  let addJSONFilePath = (self, filepath) => self.tempFiles->Array.push(TempFile.JSON(filepath))

  // read temp files and add Tokens added from "addEmacsFilePath" or "addJSONFilePath"
  let readTempFiles = async (self, editor) => {
    // read and parse and concat them
    let xs = await self.tempFiles->Array.map(TempFile.readAndParse)->Promise.all
    let tokens = xs->Array.map(snd)->Array.flat
    insert(self, editor, tokens)
    self.tempFiles = []
  }

  let clear = self => {
    // delete all unhandded temp files
    self.tempFiles->Array.forEach(format => {
      N.Fs.unlink(TempFile.toFilepath(format), _ => ())
    })
    self.agdaTokens = AVLTree.make()
  }

  let toArray = self => self.agdaTokens->AVLTree.toArray

  // for goto definition
  let lookupSrcLoc = (self, offset): option<
    promise<array<(VSCode.Range.t, Token.filepath, VSCode.Position.t)>>,
  > => {
    self.agdaTokens
    ->AVLTree.lowerBound(offset)
    ->Option.flatMap(((info, offset, range)) =>
      info.source->Option.map(((filepath, offset)) => (range, filepath, offset))
    )
    ->Option.map(((range, filepath, offset)) => {
      VSCode.Workspace.openTextDocumentWithFileName(filepath)->Promise.thenResolve(document => {
        let text = Editor.Text.getAll(document)
        let offsetConverter = Agda.OffsetConverter.make(text)
        let offset = Agda.OffsetConverter.convert(offsetConverter, offset - 1)
        let position = VSCode.TextDocument.positionAt(document, offset)
        [(range, filepath, position)]
      })
    })
  }

  let fromAspects = (editor, xs) => {
    let (semanticTokens, decorations) =
      xs
      ->Array.filterMap(((aspects, range)) => {
        // convert Aspects to TokenType / TokenModifiers / Backgrounds
        let (tokenTypeAndModifiers, decorations) =
          aspects->Array.map(Aspect.toTokenTypeAndModifiersAndDecoration)->Belt.Array.unzip
        let (tokenTypes, tokenModifiers) = tokenTypeAndModifiers->Belt.Array.unzip
        // merge TokenType / TokenModifiers / Backgrounds
        let tokenTypes = tokenTypes->Array.filterMap(x => x)
        let tokenModifiers = tokenModifiers->Array.flat
        let decorations =
          decorations->Array.filterMap(x =>
            x->Option.map(
              x => (x, Highlighting__SemanticToken.SingleLineRange.toVsCodeRange(range)),
            )
          )

        // only 1 TokenType is allowed, so we take the first one
        let semanticToken = tokenTypes[0]->Option.map(tokenType => {
          Highlighting__SemanticToken.range,
          type_: tokenType,
          modifiers: Some(tokenModifiers),
        })
        Some(semanticToken, decorations)
      })
      ->Belt.Array.unzip
    let semanticTokens = semanticTokens->Array.filterMap(x => x)
    let decorations = decorations->Array.flat->Highlighting__Decoration.toVSCodeDecorations(editor)

    (decorations, semanticTokens)
  }

  // for the new semantic highlighting
  let toDecorationsAndSemanticTokens = (tokens, editor) => {
    let xs =
      tokens
      ->toArray
      ->Array.map(((info: Token.t, offset, range)) => {
        // split the range in case that it spans multiple lines
        let ranges = Highlighting__SemanticToken.SingleLineRange.splitRange(
          VSCode.TextEditor.document(editor),
          range,
        )
        ranges->Array.map(range => (info.aspects, range))
      })
      ->Array.flat

    fromAspects(editor, xs)
  }

  // for traditional fixed-color highlighting
  let toDecorations = (self, editor): array<(Editor.Decoration.t, array<VSCode.Range.t>)> => {
    let aspects: array<(Aspect.t, VSCode.Range.t)> =
      self
      ->toArray
      ->Array.map(((info, offset, range)) =>
        // pair the aspect with the range
        info.aspects->Array.map(aspect => (aspect, range))
      )
      ->Array.flat

    aspects
    ->Array.filterMap(((aspect, range)) => Aspect.toDecoration(aspect)->Option.map(x => (x, range)))
    ->Highlighting__Decoration.toVSCodeDecorations(editor)
  }

  type action = Remove | Translate(int) | NextInterval(Intervals.t)

  // Apply edit event to the deltas, and return the updated vscodeTokens accordingly
  let applyEdit = (self, editor, event) => {
    let changes =
      event
      ->VSCode.TextDocumentChangeEvent.contentChanges
      ->Array.map(Change.fromTextDocumentContentChangeEvent)

    // update the deltas
    self.deltas = Intervals.applyChanges(self.deltas, changes)

    // generate new vscodeTokens from the new deltas and the agdaTokens
    let document = editor->VSCode.TextEditor.document
    let tokens = self->toArray

    let rec convert = (acc, tokens, i, intervals, deltaBefore) =>
      switch tokens[i] {
      | None => acc
      | Some((token, (offsetStart, offsetEnd), range)) =>
        // token WITHIN the rmeoval interval should be removed
        // token AFTER the removal interval should be translated
        let (action, deltaAfter) = switch intervals {
        | Intervals.EOF => (Translate(deltaBefore), deltaBefore)
        | Replace(removalStart, removeEnd, delta, tail) =>
          if offsetEnd < removalStart {
            //    interval         ┣━━━━━━━━━━━━━━┫
            //    token    ┣━━━━━━┫
            (Translate(deltaBefore), deltaBefore)
          } else if offsetStart < removeEnd {
            //    interval ┣━━━━━━━━━━━━━━┫
            //    token    ┣━━━━━━┫
            // this token should be removed
            (Remove, deltaBefore)
          } else {
            //    interval ┣━━━━━━━━━━━━━━┫ EOF
            //    token                     ┣━━━━━━┫
            // this token should be translated
            (NextInterval(tail), delta)
          }
        }

        switch action {
        | Remove =>
          // remove this token, move on to the next one
          convert(acc, tokens, i + 1, intervals, deltaAfter)
        | Translate(delta) =>
          let offsetStart = offsetStart + delta
          let offsetEnd = offsetEnd + delta
          let range = VSCode.Range.make(
            document->VSCode.TextDocument.positionAt(offsetStart),
            document->VSCode.TextDocument.positionAt(offsetEnd),
          )
          // split the range in case that it spans multiple lines
          let singleLineRanges = Highlighting__SemanticToken.SingleLineRange.splitRange(
            document,
            range,
          )
          let result = singleLineRanges->Array.map(range => (token.Token.aspects, range))
          convert([...acc, ...result], tokens, i + 1, intervals, deltaAfter)
        | NextInterval(tail) => convert(acc, tokens, i, tail, deltaAfter)
        }
      }

    let aspects = convert([], tokens, 0, self.deltas, 0)

    let (decorations, semanticTokens) = fromAspects(editor, aspects)

    self.vscodeTokens->Resource.set(semanticTokens)
  }

  let getVSCodeTokens = self => self.vscodeTokens
}

include Module
