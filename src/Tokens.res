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

module Intervals = {
  // For example: this is what the document would look like,
  // after the text between [12-16) has been replaced with a 6-character-long string
  //
  //    ┣━━━━━━━━━━━╋━━━ Removed ━━╋━━━ Moved 4 ━━━┫
  //    0           12             16              EOF
  //
  // And this is how it is represented with the type `t`:
  //
  // let example: t = Head(NoOp, Cons(12, Removed, Cons(16, Moved(4), Nil)))
  //

  // What happens to an interval when a change is applied to it?
  module Action = {
    type t = Removed | Moved(int)
    let toString = self =>
      switch self {
      | Removed => "━ ✘ ━"
      | Moved(0) => "━━━━━"
      | Moved(delta) =>
        if delta > 0 {
          "━ +" ++ string_of_int(delta) ++ " ━"
        } else {
          "━ -" ++ string_of_int(delta) ++ " ━"
        }
      }
  }

  module Tail = {
    type rec t = Nil | Cons(int, Action.t, t) // offset, action, tail

    let rec toString = xs =>
      switch xs {
      | Nil => "━━┫"
      | Cons(offset, action, tail) =>
        "━━┫" ++ string_of_int(offset) ++ " ━" ++ Action.toString(action) ++ toString(tail)
      }

    // start: where the previous interval ended
    // let applyChange = (start, delta, xs, change: VSCode.TextDocumentContentChangeEvent.t) =>
    //   switch xs {
    //   | Nil =>
    //     if change->VSCode.TextDocumentContentChangeEvent.rangeLength > 0 {
    //       let removedStart =
    //         change->VSCode.TextDocumentContentChangeEvent.rangeOffset
    //       let removedEnd =
    //         change->VSCode.TextDocumentContentChangeEvent.rangeOffset +
    //           change->VSCode.TextDocumentContentChangeEvent.rangeLength
    //       let insertionLength = change->VSCode.TextDocumentContentChangeEvent.text->String.length
    //       // replacement:
    //       //  moved interval: [start, removedStart)
    //       //  moved amount: delta
    //       //  removed interval: [removedStart, removedEnd)
    //       //  moved amount: insertionLength - change->VSCode.TextDocumentContentChangeEvent.rangeLength
    //       // Cons(
    //       //   start,
    //       //   Removed,
    //       //   Cons(
    //       //     removedEnd,
    //       //     Moved(insertionLength - change->VSCode.TextDocumentContentChangeEvent.rangeLength),
    //       //     Nil,
    //       //   ),
    //       // )
    //       Nil
    //     } else {
    //       // insertion only
    //       // moved amount: insertionLength
    //       Cons(start, Moved(delta), Nil)
    //     }

    //   | Cons(start, action, Nil) => xs
    //   // | Cons(start, action, Cons(end, action2, tail)) => xs
    //   }
  }

  type t = Head(Action.t, Tail.t) // the first offset is always 0

  let toString = xs =>
    switch xs {
    | Head(action, tail) => "┣━━━━━" ++ Action.toString(action) ++ Tail.toString(tail)
    }

  // let applyChange = (xs, changes: VSCode.TextDocumentChangeEvent.t) =>
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
