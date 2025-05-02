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
    "(" ++
    string_of_int(self.offset) ++
    ", " ++
    string_of_int(self.removed) ++
    ", " ++
    string_of_int(self.inserted) ++ ")"

  let fromTextDocumentContentChangeEvent = event => {
    offset: event->VSCode.TextDocumentContentChangeEvent.rangeOffset,
    removed: event->VSCode.TextDocumentContentChangeEvent.rangeLength,
    inserted: event->VSCode.TextDocumentContentChangeEvent.text->String.length,
  }

  let delta = x => x.inserted - x.removed
  let totalDelta = xs => xs->Array.reduce(0, (acc, x) => acc + delta(x))

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

  open FastCheck.Arbitrary
  // Given an offset, generates a Change.t with a random offset after that offset (within offset + 10),
  // and random removed and inserted lengths (within 0-10).
  let arbitrary = (after): arbitrary<t> => {
    integerRange(after, after + 10)->Derive.chain(offset => {
      Combinators.tuple2(integerRange(0, offset), integerRange(0, 10))->Derive.map(((
        removed,
        inserted,
      )) => {
        offset,
        removed,
        inserted,
      })
    })
  }

  // Returns an array of non-overlapping Change.t
  let arbitraryBatch = (~batchSize=?): arbitrary<array<t>> => {
    let rec aux = (after, size) => {
      if size == 0 {
        Combinators.constant([])
      } else {
        Derive.chain(arbitrary(after), change => {
          Derive.map(aux(change.offset + change.removed, size - 1), changes => {
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
  type error =
    | Overlapping // intervals are non-overlapping
    | ReversedOrder // intervals are in reversed order
    | NegativeInsertion // insertion length < 0

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
  type source =
    | Before(int) // exists in the original document at this offset before the start of change
    | Replaced(int, int, int) // doesn' exist in the original document, is at the nth offset between the start and end of a replacement
    | After
  let calculateOriginalOffset = (deltaBefore, start, end, deltaAfter, x) =>
    if x <= deltaBefore + start {
      Before(x - deltaBefore)
    } else if x > end + deltaAfter {
      After
    } else {
      Replaced(x - deltaBefore, start, end)
    }

  let rec adjustDelta = (delta, xs) =>
    switch xs {
    | EOF => EOF
    | Replace(start, end, deltaOld, tail) =>
      Replace(start, end, delta + deltaOld, adjustDelta(delta, tail))
    }

  let printInterval = (deltaBefore, start, end, deltaAfter) => {
    Js.log("interval delta before: " ++ string_of_int(deltaBefore))
    Js.log("         delta after: " ++ string_of_int(deltaAfter))
    Js.log("         start: " ++ string_of_int(start))
    Js.log("         end: " ++ string_of_int(end))
    Js.log("         removed: " ++ string_of_int(end - start))
    Js.log("         inserted: " ++ string_of_int(deltaAfter - deltaBefore + end - start))
  }

  // induction on both xs and i
  // deltaBeforeChange is `None` if it's the same as `deltaBefore`
  let rec applyChangesAux = (
    xs: t,
    changes: list<Change.t>,
    deltaBefore,
    deltaBeforeChange,
    fromBM,
  ) =>
    switch (xs, changes) {
    | (xs, list{}) => xs
    | (EOF, list{change, ...changes}) =>
      Js.log("== EOF non empty ============================================")
      // all tokens from here until EOF are translated by `deltaBefore`
      // tokens in between [change.offset, change.offset + change.removed) should be removed
      // tokens after change.offset + change.removed should be translated by `deltaBefore + change.delta`
      Js.log("channge: " ++ Change.toString(change))

      Js.log("interval delta before: " ++ string_of_int(deltaBefore))
      let deltaBefore = deltaBeforeChange->Option.getOr(deltaBefore)
      Js.log("interval delta before*: " ++ string_of_int(deltaBefore))
      let deltaAfter = deltaBefore + change.inserted - change.removed
      Js.log("interval delta after: " ++ string_of_int(deltaAfter))
      Replace(
        change.offset,
        switch fromBM {
        | None => change.offset + change.removed
        | Some(end) => end
        },
        deltaAfter,
        applyChangesAux(EOF, changes, deltaAfter, None, None),
      )
    | (Replace(start, end, deltaAfter, tail), list{change, ...changes}) =>
      // Calculate the original offset of the change
      let changeStart = calculateOriginalOffset(deltaBefore, start, end, deltaAfter, change.offset)
      let changeEnd = calculateOriginalOffset(
        deltaBefore,
        start,
        end,
        deltaAfter,
        change.offset + change.removed,
      )

      switch (changeStart, changeEnd) {
      | (Before(changeStart), Before(changeEnd)) =>
        Js.log("== Before/Before ============================================")
        printInterval(deltaBefore, start, end, deltaAfter)
        Js.log("change: " ++ Change.toString(change))
        let delta = change.inserted - change.removed
        Replace(changeStart, changeEnd, deltaBefore + delta, adjustDelta(delta, tail))
      | (Before(changeStart), Replaced(_)) =>
        Js.log("== Before/Middle ============================================")
        // let deltaBefore = deltaBeforeChange->Option.getOr(deltaBefore)
        printInterval(deltaBefore, start, end, deltaAfter)

        let insertedByInteval = deltaAfter - deltaBefore + end - start
        let removedByInterval = end - start

        Js.log("change: " ++ Change.toString(change))

        let change = {
          Change.offset: change.offset,
          removed: change.removed + change.offset - changeStart,
          inserted: change.inserted + insertedByInteval - removedByInterval,
        }

        Js.log("     => " ++ Change.toString(change))

        applyChangesAux(tail, list{change, ...changes}, deltaBefore, None, Some(end))
      | (Before(changeStart), After) =>
        Js.log("== Before/After ============================================")
        printInterval(deltaBefore, start, end, deltaAfter)
        Js.log("\nchange: " ++ Change.toString(change))
        // the change contains the interval
        // we absorb the interval and carry it forward to see how it interacts with the next change

        // use the delta before of this interval as the delta before of the change
        // unless it's specified
        let deltaBeforeChange = deltaBeforeChange->Option.getOr(deltaBefore)

        let removedByInterval = end - start
        let insertedByInterval = deltaAfter - deltaBefore + removedByInterval

        let change = {
          Change.offset: change.offset,
          removed: change.removed + removedByInterval,
          inserted: change.inserted + insertedByInterval,
        }

        Js.log("     => " ++ Change.toString(change))

        applyChangesAux(tail, list{change, ...changes}, deltaAfter, Some(deltaBeforeChange), None)

      // | (Replaced(_), Replaced(_)) => Js.Exn.raiseError("Replaced/Replaced")
      // | (Replaced(_), After) => Js.Exn.raiseError("Replaced/After")
      // | (After, After) => Js.Exn.raiseError("After/After")
      | _ => Js.Exn.raiseError("Not a possible case")
      }
    }
  let applyChanges = (xs: t, changes: array<Change.t>) =>
    applyChangesAux(xs, List.fromArray(changes), 0, None, None)
}

module type Module = {
  type t
  let toString: t => string

  let make: unit => t
  let addEmacsFilePath: (t, string) => unit
  let addJSONFilePath: (t, string) => unit
  let readTempFiles: (t, VSCode.TextEditor.t) => promise<unit>
  let insert: (t, VSCode.TextEditor.t, array<Token.t>) => unit
  let clear: t => unit

  let toArray: t => array<(Token.t, VSCode.Range.t)>

  let lookupSrcLoc: (
    t,
    int,
  ) => option<promise<array<(VSCode.Range.t, Token.filepath, VSCode.Position.t)>>>

  let toDecorations: (t, VSCode.TextEditor.t) => array<(Editor.Decoration.t, array<VSCode.Range.t>)>
  let toDecorationsAndSemanticTokens: (
    t,
    VSCode.TextEditor.t,
  ) => (array<(Editor.Decoration.t, array<VSCode.Range.t>)>, array<Highlighting__SemanticToken.t>)
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
    // Tokens indexed by the starting offset
    mutable tokens: AVLTree.t<(Token.t, VSCode.Range.t)>,
  }

  let toString = self => {
    let tempFiles = if self.tempFiles->Array.length == 0 {
      ""
    } else {
      "\n    " ++ self.tempFiles->Array.map(TempFile.toFilepath)->Array.join("\n    ")
    }

    let tokens =
      self.tokens
      ->AVLTree.toArray
      ->Array.map(((token, range)) => Token.toString(token) ++ " " ++ Editor.Range.toString(range))
      ->Array.join("\n    ")
    "Tokens:\n  tempFiles (" ++
    string_of_int(self.tempFiles->Array.length) ++
    ") " ++
    tempFiles ++
    "\n  tokens:\n    " ++
    tokens
  }

  let make = () => {
    tempFiles: [],
    tokens: AVLTree.make(),
  }

  // insert a bunch of Tokens
  // merge Aspects with the existing Token that occupies the same Range
  let insert = (self, editor, tokens: array<Token.t>) => {
    tokens->Array.forEach(info => {
      let document = editor->VSCode.TextEditor.document
      let text = Editor.Text.getAll(document)
      let offsetConverter = Agda.OffsetConverter.make(text)
      let startOffset = Agda.OffsetConverter.convert(offsetConverter, info.start)
      let existing = self.tokens->AVLTree.find(startOffset)
      switch existing {
      | None =>
        let start = VSCode.TextDocument.positionAt(document, startOffset)

        let end_ = VSCode.TextDocument.positionAt(
          document,
          Agda.OffsetConverter.convert(offsetConverter, info.end_),
        )
        let range = VSCode.Range.make(start, end_)
        self.tokens->AVLTree.insert(startOffset, (info, range))->ignore
      | Some((old, range)) =>
        // merge Aspects
        self.tokens->AVLTree.remove(startOffset)->ignore
        // often the new aspects would look exactly like the old ones
        // don't duplicate them in that case
        let newAspects =
          old.aspects == info.aspects ? old.aspects : Array.concat(old.aspects, info.aspects)
        let new = {
          ...old,
          aspects: newAspects,
        }
        self.tokens->AVLTree.insert(startOffset, (new, range))
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
    self.tokens = AVLTree.make()
  }

  let toArray = self => self.tokens->AVLTree.toArray

  // for goto definition
  let lookupSrcLoc = (self, offset): option<
    promise<array<(VSCode.Range.t, Token.filepath, VSCode.Position.t)>>,
  > => {
    self.tokens
    ->AVLTree.lowerBound(offset)
    ->Option.flatMap(((info, range)) =>
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

  // for the new semantic highlighting
  let toDecorationsAndSemanticTokens = (tokens, editor) => {
    let (semanticTokens, decorations) =
      tokens
      ->toArray
      ->Array.map(((info: Token.t, range)) => {
        // split the range in case that it spans multiple lines
        let ranges = Highlighting__SemanticToken.SingleLineRange.splitRange(
          VSCode.TextEditor.document(editor),
          range,
        )
        ranges->Array.map(range => (info.aspects, range))
      })
      ->Array.flat
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

  // for traditional fixed-color highlighting
  let toDecorations = (self, editor): array<(Editor.Decoration.t, array<VSCode.Range.t>)> => {
    let aspects: array<(Aspect.t, VSCode.Range.t)> =
      self
      ->toArray
      ->Array.map(((info, range)) =>
        // pair the aspect with the range
        info.aspects->Array.map(aspect => (aspect, range))
      )
      ->Array.flat

    aspects
    ->Array.filterMap(((aspect, range)) => Aspect.toDecoration(aspect)->Option.map(x => (x, range)))
    ->Highlighting__Decoration.toVSCodeDecorations(editor)
  }
}

include Module
