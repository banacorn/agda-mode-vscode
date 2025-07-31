// Import extracted modules
module Aspect = Highlighting__AgdaAspect

// Re-export types for backward compatibility
type agdaOffset = Token.agdaOffset
type vscodeOffset = Token.vscodeOffset

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
    mutable deltas: TokenIntervals.t,
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
      ->Array.map(((_, token)) =>
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
    deltas: TokenIntervals.empty,
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
    self.deltas = TokenIntervals.empty

    // reset the vscodeTokens (only when it has ever been set)
    if self.vscodeTokens->Resource.isPending {
      ()
    } else {
      self.vscodeTokens->Resource.set([])
    }
  }

  let toTokenArray = self => self.agdaTokens->AVLTree.toArray->Array.map(snd)
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
        | TokenIntervals.EOF =>
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
      ->Array.map(TokenChange.fromTextDocumentContentChangeEvent)

    // update the deltas
    self.deltas = TokenIntervals.applyChanges(self.deltas, changes->Array.toReversed)
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
