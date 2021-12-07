open Belt

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
    "Token (" ++
    string_of_int(self.start) ++
    ", " ++
    string_of_int(self.end_) ++
    ") " ++
    Util.Pretty.list(List.fromArray(self.aspects)) ++
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
            int_of_string_opt(index')->Option.map(index => {
              start: start - 1,
              end_: end_ - 1,
              aspects: flatten(aspects)->Array.map(Aspect.parse),
              isTokenBased: false, // NOTE: fix this
              note: None, // NOTE: fix this
              source: Some((filepath, index)),
            })
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
    tokens->Js.Array.sliceFrom(2, _)->Array.map(parse)->Array.keepMap(x => x)

  // from JSON
  let decode: Json.Decode.decoder<t> = {
    open Json.Decode
    Util.Decode.tuple6(
      int,
      int,
      array(string),
      bool,
      optional(string),
      optional(pair(string, int)),
    ) |> map(((start, end_, aspects, isTokenBased, note, source)) => {
      start: start - 1,
      end_: end_ - 1,
      aspects: aspects->Array.map(Aspect.parse),
      isTokenBased: isTokenBased,
      note: note,
      source: source,
    })
  }

  // from JSON
  let decodeResponseHighlightingInfoDirect: Json.Decode.decoder<(bool, array<t>)> = {
    open Json.Decode
    pair(bool, array(decode)) |> map(((keepHighlighting, xs)) => (keepHighlighting, xs))
  }
}

module type Module = {
  type t

  let make: unit => t
  let addEmacsFilePath: (t, string) => unit
  let addJSONFilePath: (t, string) => unit
  let readTempFiles: (t, VSCode.TextEditor.t) => Promise.promise<unit>
  let insert: (t, VSCode.TextEditor.t, array<Token.t>) => unit
  let clear: t => unit

  let toArray: t => array<(Token.t, VSCode.Range.t)>

  let lookupSrcLoc: (
    t,
    int,
  ) => option<Promise.t<array<(VSCode.Range.t, Token.filepath, VSCode.Position.t)>>>

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

    let readAndParse = format => {
      N.Util.promisify(N.Fs.readFile)(. toFilepath(format))
      ->Promise.Js.fromBsPromise
      ->Promise.Js.toResult
      ->Promise.map(x =>
        switch x {
        | Ok(buffer) =>
          switch format {
          | Emacs(_) =>
            let tokens = switch Parser.SExpression.parse(Node.Buffer.toString(buffer))[0] {
            | Some(Ok(L(xs))) => xs
            | _ => []
            }
            // RemoveTokenBasedHighlighting
            let removeTokenBasedHighlighting = switch tokens[0] {
            | Some(A("remove")) => true
            | _ => false
            }
            let tokens = Js.Array.sliceFrom(1, tokens)->Array.keepMap(Token.parse)
            (removeTokenBasedHighlighting, tokens)
          | JSON(_) =>
            let raw = Node.Buffer.toString(buffer)
            switch Js.Json.parseExn(raw) {
            | exception _e => (false, [])
            | json => Token.decodeResponseHighlightingInfoDirect(json)
            }
          }
        | Error(_err) => (false, [])
        }
      )
    }
  }

  type t = {
    // from addEmacsFilePath/addJSONFilePath
    mutable tempFiles: array<TempFile.t>,
    // Tokens indexed by the starting offset
    mutable tokens: AVLTree.t<(Token.t, VSCode.Range.t)>,
  }

  let make = () => {
    tempFiles: [],
    tokens: AVLTree.make(),
  }

  // insert a bunch of Tokens
  // merge Aspects with the existing Token that occupies the same Range
  let insert = (self, editor, tokens: array<Token.t>) => {
    tokens->Array.forEach(info => {
      let existing = self.tokens->AVLTree.find(info.start)
      switch existing {
      | None =>
        let document = editor->VSCode.TextEditor.document
        let text = Editor.Text.getAll(document)
        let offsetConverter = Agda.OffsetConverter.make(text)
        let start = Editor.Position.fromOffset(
          document,
          Agda.OffsetConverter.convert(offsetConverter, info.start),
        )
        let end_ = Editor.Position.fromOffset(
          document,
          Agda.OffsetConverter.convert(offsetConverter, info.end_),
        )
        let range = VSCode.Range.make(start, end_)
        self.tokens->AVLTree.insert(info.start, (info, range))->ignore
      | Some((old, range)) =>
        // merge Aspects
        self.tokens->AVLTree.remove(info.start)->ignore
        // often the new aspects would look exactly like the old ones
        // don't duplicate them in that case
        let newAspects =
          old.aspects == info.aspects ? old.aspects : Array.concat(old.aspects, info.aspects)
        let new = {
          ...old,
          aspects: newAspects,
        }
        self.tokens->AVLTree.insert(info.start, (new, range))->ignore
      }
    })
  }

  let addEmacsFilePath = (self, filepath) =>
    Js.Array.push(TempFile.Emacs(filepath), self.tempFiles)->ignore
  let addJSONFilePath = (self, filepath) =>
    Js.Array.push(TempFile.JSON(filepath), self.tempFiles)->ignore

  // read temp files and add Tokens added from "addEmacsFilePath" or "addJSONFilePath"
  let readTempFiles = (self, editor) => {
    // read and parse and concat them
    self.tempFiles
    ->Array.map(TempFile.readAndParse)
    ->Promise.allArray
    ->Promise.map(xs => xs->Array.map(snd)->Array.concatMany)
    ->Promise.map(tokens => {
      insert(self, editor, tokens)
      self.tempFiles = []
    })
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
    Promise.t<array<(VSCode.Range.t, Token.filepath, VSCode.Position.t)>>,
  > => {
    self.tokens
    ->AVLTree.lowerBound(offset)
    ->Option.flatMap(((info, range)) =>
      info.source->Option.map(((filepath, offset)) => (range, filepath, offset))
    )
    ->Option.map(((range, filepath, offset)) => {
      VSCode.Workspace.openTextDocumentWithFileName(filepath)->Promise.map(document => {
        let text = Editor.Text.getAll(document)
        let offsetConverter = Agda.OffsetConverter.make(text)
        let offset = Agda.OffsetConverter.convert(offsetConverter, offset - 1)
        let position = Editor.Position.fromOffset(document, offset)
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
      ->Array.concatMany
      ->Array.keepMap(((aspects, range)) => {
        // convert Aspects to TokenType / TokenModifiers / Backgrounds
        let (tokenTypeAndModifiers, decorations) =
          aspects->Array.map(Aspect.toTokenTypeAndModifiersAndDecoration)->Array.unzip
        let (tokenTypes, tokenModifiers) = tokenTypeAndModifiers->Array.unzip
        // merge TokenType / TokenModifiers / Backgrounds
        let tokenTypes = tokenTypes->Array.keepMap(x => x)
        let tokenModifiers = tokenModifiers->Array.concatMany
        let decorations =
          decorations->Array.keepMap(x =>
            x->Option.map(x => (
              x,
              Highlighting__SemanticToken.SingleLineRange.toVsCodeRange(range),
            ))
          )

        // only 1 TokenType is allowed, so we take the first one
        let semanticToken = tokenTypes[0]->Option.map(tokenType => {
          Highlighting__SemanticToken.range: range,
          type_: tokenType,
          modifiers: Some(tokenModifiers),
        })
        Some(semanticToken, decorations)
      })
      ->Array.unzip
    let semanticTokens = semanticTokens->Array.keepMap(x => x)
    let decorations =
      decorations->Array.concatMany->Highlighting__Decoration.toVSCodeDecorations(editor)

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
      ->Array.concatMany

    aspects
    ->Array.keepMap(((aspect, range)) => Aspect.toDecoration(aspect)->Option.map(x => (x, range)))
    ->Highlighting__Decoration.toVSCodeDecorations(editor)
  }
}

include Module
