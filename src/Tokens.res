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

  open FastCheck.Arbitrary
  // Given an offset, generates a Change.t with a random offset after that offset (within offset + 10),
  // and random removed and inserted lengths (within 0-10).
  let arbitrary = (after): arbitrary<t> => {
    Derive.chain(integerRange(after, after + 10), offset => {
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
  let arbitraryBatch = (): arbitrary<array<t>> => {
    let rec aux = (after, size) => {
      if size == 0 {
        Combinators.constant([])
      } else {
        Derive.chain(arbitrary(after), change => {
          Derive.map(aux(change.offset + change.inserted - change.removed, size - 1), changes => {
            [change, ...changes]
          })
        })
      }
    }

    Derive.chain(integerRange(0, 10), size => aux(0, size))
  }
}

module Intervals = {
  // For example: this is what the document would look like,
  // after the text between [12-16) has been replaced with a 6-character-long string
  //
  //    ┣━━━━━━━━━━━╋━━━ Removed ━━╋━━━ Moved 4 ━━━┫
  //    0           12             16              EOF
  //
  // And this is how it is represented with the type `t`:
  //
  // let example: t = Head(0, 0, Replace(12, 16, 4, Nil))
  //

  let deltaToString = delta =>
    if delta > 0 {
      "━ +" ++ string_of_int(delta) ++ " ━"
    } else if delta < 0 {
      "━ " ++ string_of_int(delta) ++ " ━"
    } else {
      "━━━━━"
    }

  module Tail = {
    type rec t =
      | EOF
      | Replace(int, int, int, t) // start of replacement, end of replacement, delta so far, tail

    let rec toString = xs =>
      switch xs {
      | EOF => "━━┫"
      | Replace(start, end, delta, tail) =>
        if start == end {
          "━━┫" ++ string_of_int(end) ++ " ━" ++ deltaToString(delta) ++ toString(tail)
        } else {
          "━━┫" ++
          string_of_int(start) ++
          "     ┃" ++
          string_of_int(end) ++
          " ━━" ++
          deltaToString(delta) ++
          toString(tail)
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

    // let applyChange = (xs, change: Change.t) =>
    //   switch xs {
    //   | EOF => EOF
    //   | _ => EOF
    //   }
  }

  type t = Head(int, int, Tail.t) // like Tail.Replace except that the first offset is always 0

  let empty = Head(0, 0, Tail.EOF)

  let toString = xs =>
    switch xs {
    | Head(0, delta, tail) => "┣━" ++ deltaToString(delta) ++ Tail.toString(tail)
    | Head(end, delta, tail) =>
      "┣━━━━━┫" ++
      string_of_int(end) ++
      " ━━" ++
      deltaToString(delta) ++
      Tail.toString(tail)
    }

  // For testing: the last delta should be the total delta
  let totalDelta = xs =>
    switch xs {
    | Head(_, delta, EOF) => delta
    | Head(_, _, tail) => Tail.totalDelta(tail)
    }

  // For testing: returns an array of removed intervals
  let removedIntervals = xs =>
    switch xs {
    | Head(0, _, tail) => Tail.removedIntervals(tail)
    | Head(end, _, tail) => [(0, end), ...Tail.removedIntervals(tail)]
    }

  let applyChange = (xs, change: Change.t) => {
    switch xs {
    | Head(0, 0, tail) =>
      let delta = change.inserted - change.removed
      if change.offset == 0 {
        Head(0, delta, tail)
      } else {
        Head(0, 0, Replace(change.offset, change.offset + change.removed, delta, tail))
      }
    | Head(end, delta, tail) => Head(end, delta, tail)
    }
  }

  // let applyChange = (xs, change: VSCode.TextDocumentContentChangeEvent.t) =>
  //   switch xs {
  //   | Head(action, tail) => Head(action, Tail.applyChange(0, 0, tail, change))
  //   }
  // let applyChanges = (xs, changes)
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
