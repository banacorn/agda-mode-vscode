open VSCode
module VSRange = Range

module Position = {
  let toString = position =>
    string_of_int(VSCode.Position.line(position)) ++
    ":" ++
    string_of_int(VSCode.Position.character(position))
}

module Range = {
  let toString = range => {
    let start = VSCode.Range.start(range)
    let end = VSCode.Range.end_(range)
    if VSCode.Position.line(start) == VSCode.Position.line(end) {
      string_of_int(VSCode.Position.line(start)) ++
      ":" ++
      string_of_int(VSCode.Position.character(start)) ++
      "-" ++
      string_of_int(VSCode.Position.character(end))
    } else {
      Position.toString(start) ++ "-" ++ Position.toString(end)
    }
  }
}

module Decoration = {
  type t = TextEditorDecorationType.t
  type backgroundStyle = string
  type foregroundStyle = string
  type color = string

  let decorate = (editor: TextEditor.t, decoration: t, ranges: array<VSRange.t>) =>
    editor->TextEditor.setDecorations(decoration, ranges)

  let highlightBackgroundPrim = (
    editor: TextEditor.t,
    backgroundColor: VSCode.StringOr.t<ThemeColor.t>,
    ranges: array<VSRange.t>,
  ) => {
    let rangeBehavior = DecorationRangeBehavior.ClosedClosed
    let options = DecorationRenderOptions.t(~backgroundColor, ~rangeBehavior, ())
    let decoration = Window.createTextEditorDecorationType(options)
    editor->decorate(decoration, ranges)
    decoration
  }
  let highlightBackground = (
    editor: TextEditor.t,
    style: backgroundStyle,
    ranges: array<VSRange.t>,
  ) => highlightBackgroundPrim(editor, VSCode.StringOr.others(ThemeColor.make(style)), ranges)

  let highlightBackgroundWithColor = (
    editor: TextEditor.t,
    color: color,
    ranges: array<VSRange.t>,
  ) => highlightBackgroundPrim(editor, VSCode.StringOr.string(color), ranges)

  let decorateTextPrim = (
    editor: TextEditor.t,
    color: VSCode.StringOr.t<ThemeColor.t>,
    ranges: array<VSRange.t>,
  ) => {
    let rangeBehavior = DecorationRangeBehavior.ClosedClosed
    let options = DecorationRenderOptions.t(~color, ~rangeBehavior, ())
    let decoration = Window.createTextEditorDecorationType(options)
    editor->decorate(decoration, ranges)
    decoration
  }
  let decorateText = (editor: TextEditor.t, style: backgroundStyle, ranges: array<VSRange.t>) =>
    decorateTextPrim(editor, VSCode.StringOr.others(ThemeColor.make(style)), ranges)

  let decorateTextWithColor = (editor: TextEditor.t, color: color, ranges: array<VSRange.t>) =>
    decorateTextPrim(editor, VSCode.StringOr.string(color), ranges)

  let overlayTextPrim = (
    editor: TextEditor.t,
    color: VSCode.StringOr.t<ThemeColor.t>,
    text: string,
    range: VSRange.t,
  ) => {
    let after = ThemableDecorationAttachmentRenderOptions.t(~contentText=text, ~color, ())

    let options = DecorationRenderOptions.t(~after, ())
    let decoration = Window.createTextEditorDecorationType(options)
    editor->decorate(decoration, [range])
    decoration
  }

  let overlayText = (
    editor: TextEditor.t,
    style: foregroundStyle,
    text: string,
    range: VSRange.t,
  ) => overlayTextPrim(editor, VSCode.StringOr.others(ThemeColor.make(style)), text, range)

  let overlayTextWithColor = (editor: TextEditor.t, color: color, text: string, range: VSRange.t) =>
    overlayTextPrim(editor, VSCode.StringOr.string(color), text, range)

  let underlineText = (editor: TextEditor.t, range: VSRange.t) => {
    let rangeBehavior = DecorationRangeBehavior.ClosedOpen
    let textDecoration = "underline dotted"
    let options = DecorationRenderOptions.t(~rangeBehavior, ~textDecoration, ())
    let decoration = Window.createTextEditorDecorationType(options)
    editor->decorate(decoration, [range])
    decoration
  }

  let destroy = TextEditorDecorationType.dispose
}

module Cursor = {
  let set = (editor, point) => {
    let selection = Selection.make(point, point)
    editor->TextEditor.setSelection(selection)
  }
  let setMany = (editor, points) => {
    let selections = points->Array.map(point => Selection.make(point, point))
    editor->TextEditor.setSelections(selections)
  }
  let get = editor => editor->TextEditor.selection->Selection.active
  let getMany = editor => editor->TextEditor.selections->Array.map(Selection.active)
}

module Selection = {
  let set = (editor, range) => {
    let selection = Selection.make(VSRange.start(range), VSRange.end_(range))
    editor->TextEditor.setSelection(selection)
  }
  let setMany = (editor, ranges) => {
    let selections =
      ranges->Array.map(range => Selection.make(VSRange.start(range), VSRange.end_(range)))
    editor->TextEditor.setSelections(selections)
  }
  let get = editor => {
    let selection = editor->TextEditor.selection
    VSRange.make(Selection.start(selection), Selection.end_(selection))
  }
  let getMany = editor =>
    editor
    ->TextEditor.selections
    ->Array.map(selection => VSRange.make(Selection.start(selection), Selection.end_(selection)))
}

module Text = {
  let get = (document, range) => document->TextDocument.getText(Some(range))
  let getAll = document => document->TextDocument.getText(None)

  let replace = (document, range, text) => {
    let workspaceEdit = WorkspaceEdit.make()
    workspaceEdit->WorkspaceEdit.replace(document->TextDocument.uri, range, text, None)
    Workspace.applyEdit(workspaceEdit)
  }
  let batchReplace = (document, replacements) => {
    let workspaceEdit = WorkspaceEdit.make()
    replacements->Array.forEach(((range, text)) =>
      workspaceEdit->WorkspaceEdit.replace(document->TextDocument.uri, range, text, None)
    )
    Workspace.applyEdit(workspaceEdit)
  }
  // let batchReplace' = (editor, replacements) => {
  //   editor->TextEditor.edit(editBuilder => {
  //     replacements->Array.forEach(((range, text)) =>
  //       editBuilder->TextEditorEdit.replaceAtRange(range, text)
  //     )
  //   }, None)
  // }

  let insert = (document, point, text) => {
    let workspaceEdit = WorkspaceEdit.make()
    workspaceEdit->WorkspaceEdit.insert(document->TextDocument.uri, point, text, None)
    Workspace.applyEdit(workspaceEdit)
  }
  let batchInsert = (document, points, text) => {
    let workspaceEdit = WorkspaceEdit.make()
    let textEdits = points->Array.map(point => TextEdit.insert(point, text))
    workspaceEdit->WorkspaceEdit.set(document->TextDocument.uri, textEdits)
    Workspace.applyEdit(workspaceEdit)
  }
  // let batchInsert' = (editor, points, text) => {
  //   editor->TextEditor.edit(editBuilder => {
  //     points->Array.forEach(point => editBuilder->TextEditorEdit.insert(point, text))
  //   }, None)
  // }
  let delete = (document, range) => {
    let workspaceEdit = WorkspaceEdit.make()
    workspaceEdit->WorkspaceEdit.delete(document->TextDocument.uri, range, None)
    Workspace.applyEdit(workspaceEdit)
  }
}

let focus = document => Window.showTextDocument(document, ~column=ViewColumn.One, ())->ignore

let reveal = (editor, range) =>
  editor->TextEditor.revealRange(range, Some(TextEditorRevealType.InCenterIfOutsideViewport))

module Provider = {
  let documentSelector = [
    VSCode.StringOr.string("agda"),
    VSCode.StringOr.string("lagda-md"),
    VSCode.StringOr.string("lagda-rst"),
    VSCode.StringOr.string("lagda-tex"),
  ]
  let registerDefinitionProvider = definitionProvider => {
    open DefinitionProvider
    Languages.registerDefinitionProvider(
      documentSelector,
      {
        provideDefinition: (textDocument, point, _) =>
          definitionProvider(
            textDocument->TextDocument.fileName,
            point,
          )->ProviderResult.map(pairs =>
            LocationLinkOrLocation.locationLinks(
              pairs->Array.map(((srcRange, targetFile, targetPos)) => {
                open LocationLink
                {
                  originSelectionRange: Some(srcRange),
                  targetRange: VSRange.make(targetPos, targetPos),
                  targetSelectionRange: None,
                  targetUri: Uri.file(targetFile),
                }
              }),
            )
          ),
      },
    )
  }

  let registerHoverProvider = hoverProvider => {
    open HoverProvider
    Languages.registerHoverProvider(
      documentSelector,
      {
        {
          provideHover: (textDocument, point, _) =>
            hoverProvider(textDocument->TextDocument.fileName, point)->ProviderResult.map(((
              strings,
              range,
            )) => {
              let markdownStrings = strings->Array.map(string => MarkdownString.make(string, true))
              Hover.makeWithRange(markdownStrings, range)
            }),
        }
      },
    )
  }

  module Mock = {
    // https://code.visualstudio.com/api/references/vscode-api#SemanticsTokens
    module SemanticsTokens = {
      type t
      // constructors
      @module("vscode") @new
      external make: array<int> => t = "SemanticsTokens"
      @module("vscode") @new
      external makeWithResultId: (array<int>, string) => t = "SemanticsTokens"
      // properties
      @get external data: t => array<int> = "data"
      @get external resultId: t => option<string> = "resultId"
    }

    // https://code.visualstudio.com/api/references/vscode-api#SemanticTokensEdit
    module SemanticTokensEdit = {
      type t
      // constructors
      @module("vscode") @new
      external make: (int, int) => t = "SemanticTokensEdit"
      @module("vscode") @new
      external makeWithData: (int, int, array<int>) => t = "SemanticTokensEdit"
      // properties
      @get external data: t => option<array<int>> = "data"
      @get external deleteCount: t => int = "deleteCount"
      @get external start: t => int = "start"
    }

    // https://code.visualstudio.com/api/references/vscode-api#SemanticTokensEdits
    module SemanticTokensEdits = {
      type t
      // constructors
      @module("vscode") @new
      external make: array<SemanticTokensEdit.t> => t = "SemanticTokensEdits"
      @module("vscode") @new
      external makeWithResultId: (array<SemanticTokensEdit.t>, string) => t = "SemanticTokensEdits"
      // properties
      @get external edits: t => array<SemanticTokensEdit.t> = "edits"
      @get external resultId: t => option<string> = "resultId"
    }

    // https://code.visualstudio.com/api/references/vscode-api#SemanticTokensLegend
    module SemanticTokensLegend = {
      type t
      // constructors
      @module("vscode") @new
      external make: array<string> => t = "SemanticTokensLegend"
      @module("vscode") @new
      external makeWithTokenModifiers: (array<string>, array<string>) => t = "SemanticTokensLegend"
      // properties
      @get
      external tokenModifiers: t => array<string> = "tokenModifiers"
      @get external tokenTypes: t => array<string> = "tokenTypes"
    }

    // https://code.visualstudio.com/api/references/vscode-api#SemanticTokensBuilder
    module SemanticTokensBuilder = {
      type t
      // constructors
      @module("vscode") @new
      external make: unit => t = "SemanticTokensBuilder"
      @module("vscode") @new
      external makeWithLegend: SemanticTokensLegend.t => t = "SemanticTokensBuilder"
      // methods
      @send external build: t => SemanticsTokens.t = "build"
      @send
      external buildWithResultId: (t, string) => SemanticsTokens.t = "build"
      @send
      external push: (t, int, int, int, int, option<int>) => unit = "push"
      @send
      external pushLegend: (t, VSRange.t, string, option<array<string>>) => unit = "push"
    }

    // https://code.visualstudio.com/api/references/vscode-api#DocumentSemanticTokensProvider
    module DocumentSemanticTokensProvider = {
      // missing: onDidChangeSemanticTokens

      type provideDocumentSemanticTokens = (
        TextDocument.t,
        CancellationToken.t,
      ) => ProviderResult.t<SemanticsTokens.t>

      type provideDocumentSemanticTokensEdits = (
        TextDocument.t,
        string,
        CancellationToken.t,
      ) => ProviderResult.t<
        @unwrap
        [
          | #SemanticsTokens(SemanticsTokens.t)
          | #SemanticTokensEdits(SemanticTokensEdits.t)
        ],
      >

      type t = {
        provideDocumentSemanticTokens: option<provideDocumentSemanticTokens>,
        provideDocumentSemanticTokensEdits: option<provideDocumentSemanticTokensEdits>,
      }

      let make = (
        ~provideDocumentSemanticTokens: option<provideDocumentSemanticTokens>=?,
        ~provideDocumentSemanticTokensEdits: option<provideDocumentSemanticTokensEdits>=?,
        (),
      ) => {
        provideDocumentSemanticTokens,
        provideDocumentSemanticTokensEdits,
      }
    }

    module Languages = {
      @module("vscode") @scope("languages")
      external registerDocumentSemanticTokensProvider: (
        DocumentSelector.t,
        DocumentSemanticTokensProvider.t,
        SemanticTokensLegend.t,
      ) => Disposable.t = "registerDocumentSemanticTokensProvider"
    }
  }

  let registerDocumentSemanticTokensProvider = (
    ~provideDocumentSemanticTokens: Mock.DocumentSemanticTokensProvider.provideDocumentSemanticTokens,
    (tokenTypes, tokenModifiers),
  ) => {
    let documentSemanticTokensProvider = Mock.DocumentSemanticTokensProvider.make(
      ~provideDocumentSemanticTokens,
      // =(textDocument, _cancel) => {
      //   let builder = Mock.SemanticTokensBuilder.makeWithLegend(semanticTokensLegend)
      //   let pushLegend = (range, tokenType, tokenModifiers) => {
      //     Mock.SemanticTokensBuilder.pushLegend(
      //       builder,
      //       range,
      //       Highlighting.Aspect.TokenType.toString(tokenType),
      //       tokenModifiers->Option.map(xs =>
      //         xs->Array.map(Highlighting.Aspect.TokenModifier.toString)
      //       ),
      //     )
      //   }
      //   provider(textDocument->TextDocument.fileName, pushLegend)->ProviderResult.map(() => {
      //     Mock.SemanticTokensBuilder.build(builder)
      //   })
      // },
      // NOTE: this provider is never invokes somehow
      // ~provideDocumentSemanticTokensEdits=None,
      // ~provideDocumentSemanticTokensEdits=(textDocument, _previousResultID, _cancel) => {
      //   let builder = Mock.SemanticTokensBuilder.makeWithLegend(semanticTokensLegend)
      //   let pushLegend = (range, tokenType, tokenModifiers) => {
      //     Mock.SemanticTokensBuilder.pushLegend(
      //       builder,
      //       range,
      //       Highlighting.Aspect.TokenType.toString(tokenType),
      //       tokenModifiers->Option.map(xs =>
      //         xs->Array.map(Highlighting.Aspect.TokenModifier.toString)
      //       ),
      //     )
      //   }
      //   provider(textDocument->TextDocument.fileName, pushLegend)->ProviderResult.map(() => {
      //     #SemanticsTokens(Mock.SemanticTokensBuilder.build(builder))
      //   })
      // },
      (),
    )

    let semanticTokensLegend = Mock.SemanticTokensLegend.makeWithTokenModifiers(
      tokenTypes,
      tokenModifiers,
    )

    Mock.Languages.registerDocumentSemanticTokensProvider(
      documentSelector,
      documentSemanticTokensProvider,
      semanticTokensLegend,
    )
  }
}

let toUTF8Offset = (document, offset) => {
  let range = VSRange.make(VSCode.Position.make(0, 0), document->TextDocument.positionAt(offset)) // start // end
  let text = document->TextDocument.getText(Some(range))
  Agda.OffsetConverter.characterWidth(text)
}
