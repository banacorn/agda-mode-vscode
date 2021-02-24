open VSCode
module VSRange = Range
open Belt

module Position = {
  let fromOffset = (document, offset) => document->VSCode.TextDocument.positionAt(offset)

  let toOffset = (document, position) => document->VSCode.TextDocument.offsetAt(position)

  let fromAgdaPosition = (position: Common.Agda.Position.t) =>
    VSCode.Position.make(position.line - 1, position.col - 1)
}

module Range = {
  let fromInterval = (document, interval) =>
    VSCode.Range.make(
      Position.fromOffset(document, fst(interval)),
      Position.fromOffset(document, snd(interval)),
    )
  let toInterval = (document, range) => (
    Position.toOffset(document, VSCode.Range.start(range)),
    Position.toOffset(document, VSCode.Range.end_(range)),
  )

  let fromAgdaInterval = (range: Common.Agda.Interval.t) =>
    VSCode.Range.make(Position.fromAgdaPosition(range.start), Position.fromAgdaPosition(range.end_))
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
    let rangeBehavior = DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed)
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
    let rangeBehavior = DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed)
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
    let rangeBehavior = DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedOpen)
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
  let batchReplace' = (editor, replacements) => {
    editor->TextEditor.edit(editBuilder => {
      replacements->Array.forEach(((range, text)) =>
        editBuilder->TextEditorEdit.replaceAtRange(range, text)
      )
    }, None)
  }

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
  let batchInsert' = (editor, points, text) => {
    editor->TextEditor.edit(editBuilder => {
      points->Array.forEach(point => editBuilder->TextEditorEdit.insert(point, text))
    }, None)
  }
  let delete = (document, range) => {
    let workspaceEdit = WorkspaceEdit.make()
    workspaceEdit->WorkspaceEdit.delete(document->TextDocument.uri, range, None)
    Workspace.applyEdit(workspaceEdit)
  }
}

let focus = document => Window.showTextDocument(document, ~column=ViewColumn.Beside, ())->ignore

let reveal = (editor, range) =>
  editor->TextEditor.revealRange(range, Some(TextEditorRevealType.InCenterIfOutsideViewport))

module Provider = {
  let documentSelector = [VSCode.StringOr.string("agda")]
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
              let markdownStrings =
                strings->Belt.Array.map(string => MarkdownString.make(string, true))
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
      @bs.module("vscode") @bs.new
      external make: array<int> => t = "SemanticsTokens"
      @bs.module("vscode") @bs.new
      external makeWithResultId: (array<int>, string) => t = "SemanticsTokens"
      // properties
      @bs.get external data: t => array<int> = "data"
      @bs.get external resultId: t => option<string> = "resultId"
    }

    // https://code.visualstudio.com/api/references/vscode-api#SemanticTokensLegend
    module SemanticTokensLegend = {
      type t
      // constructors
      @bs.module("vscode") @bs.new
      external make: array<string> => t = "SemanticTokensLegend"
      @bs.module("vscode") @bs.new
      external makeWithTokenModifiers: (array<string>, array<string>) => t = "SemanticTokensLegend"
      // properties
      @bs.get
      external tokenModifiers: t => array<string> = "tokenModifiers"
      @bs.get external tokenTypes: t => array<string> = "tokenTypes"
    }

    // https://code.visualstudio.com/api/references/vscode-api#SemanticTokensBuilder
    module SemanticTokensBuilder = {
      type t
      // constructors
      @bs.module("vscode") @bs.new
      external make: unit => t = "SemanticTokensBuilder"
      @bs.module("vscode") @bs.new
      external makeWithLegend: SemanticTokensLegend.t => t = "SemanticTokensBuilder"
      // methods
      @bs.send external build: t => SemanticsTokens.t = "build"
      @bs.send
      external buildWithResultId: (t, string) => SemanticsTokens.t = "build"
      @bs.send
      external push: (t, int, int, int, int, option<int>) => unit = "push"
      @bs.send
      external pushLegend: (t, VSRange.t, string, option<array<string>>) => unit = "push"
    }

    // https://code.visualstudio.com/api/references/vscode-api#DocumentSemanticTokensProvider
    module DocumentSemanticTokensProvider = {
      // missing: onDidChangeSemanticTokens
      // missing: provideDocumentSemanticTokensEdits
      type t = {
        provideDocumentSemanticTokens: (
          TextDocument.t,
          CancellationToken.t,
        ) => ProviderResult.t<SemanticsTokens.t>,
      }
    }

    module Languages = {
      @bs.module("vscode") @bs.scope("languages")
      external registerDocumentSemanticTokensProvider: (
        DocumentSelector.t,
        DocumentSemanticTokensProvider.t,
        SemanticTokensLegend.t,
      ) => Disposable.t = "registerDocumentSemanticTokensProvider"
    }
  }

  let registerSemnaticTokenProvider = (prodider, (tokenTypes, tokenModifiers)) => {
    let semanticTokensLegend = Mock.SemanticTokensLegend.makeWithTokenModifiers(
      tokenTypes,
      tokenModifiers,
    )

    let documentSemanticTokensProvider = {
      open Mock.DocumentSemanticTokensProvider
      {
        provideDocumentSemanticTokens: (textDocument, _) => {
          let builder = Mock.SemanticTokensBuilder.makeWithLegend(semanticTokensLegend)
          let pushLegend = (range, tokenType, tokenModifiers) => {
            Mock.SemanticTokensBuilder.pushLegend(
              builder,
              range,
              Highlighting.Aspect.TokenType.toString(tokenType),
              tokenModifiers->Option.map(xs =>
                xs->Array.map(Highlighting.Aspect.TokenModifier.toString)
              ),
            )
          }
          prodider(textDocument->TextDocument.fileName, pushLegend)->ProviderResult.map(() =>
            Mock.SemanticTokensBuilder.build(builder)
          )
        },
      }
    }

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
  Common.Agda.OffsetConverter.characterWidth(text)
}
