open VSCode;
module VSRange = Range;
open Belt;

module Decoration = {
  type t = TextEditorDecorationType.t;
  type backgroundStyle = string;
  type foregroundStyle = string;
  type color = string;

  let decorate =
      (editor: TextEditor.t, decoration: t, ranges: array(VSRange.t)) => {
    editor->TextEditor.setDecorations(decoration, ranges);
  };
  //
  let highlightBackgroundPrim =
      (
        editor: TextEditor.t,
        backgroundColor: VSCode.StringOr.t(ThemeColor.t),
        ranges: array(VSRange.t),
      ) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed);
    let options =
      DecorationRenderOptions.t(~backgroundColor, ~rangeBehavior, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, ranges);
    decoration;
  };
  let highlightBackground =
      (
        editor: TextEditor.t,
        style: backgroundStyle,
        ranges: array(VSRange.t),
      ) =>
    highlightBackgroundPrim(
      editor,
      VSCode.StringOr.others(ThemeColor.make(style)),
      ranges,
    );

  let highlightBackgroundWithColor =
      (editor: TextEditor.t, color: color, ranges: array(VSRange.t)) =>
    highlightBackgroundPrim(editor, VSCode.StringOr.string(color), ranges);

  let decorateTextPrim =
      (
        editor: TextEditor.t,
        color: VSCode.StringOr.t(ThemeColor.t),
        ranges: array(VSRange.t),
      ) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed);
    let options = DecorationRenderOptions.t(~color, ~rangeBehavior, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, ranges);
    decoration;
  };
  let decorateText =
      (
        editor: TextEditor.t,
        style: backgroundStyle,
        ranges: array(VSRange.t),
      ) =>
    decorateTextPrim(
      editor,
      VSCode.StringOr.others(ThemeColor.make(style)),
      ranges,
    );

  let decorateTextWithColor =
      (editor: TextEditor.t, color: color, ranges: array(VSRange.t)) =>
    decorateTextPrim(editor, VSCode.StringOr.string(color), ranges);

  let overlayTextPrim =
      (
        editor: TextEditor.t,
        color: VSCode.StringOr.t(ThemeColor.t),
        text: string,
        range: VSRange.t,
      ) => {
    let after =
      ThemableDecorationAttachmentRenderOptions.t(
        ~contentText=text,
        ~color,
        (),
      );

    let options = DecorationRenderOptions.t(~after, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, [|range|]);
    decoration;
  };

  let overlayText =
      (
        editor: TextEditor.t,
        style: foregroundStyle,
        text: string,
        range: VSRange.t,
      ) =>
    overlayTextPrim(
      editor,
      VSCode.StringOr.others(ThemeColor.make(style)),
      text,
      range,
    );

  let overlayTextWithColor =
      (editor: TextEditor.t, color: color, text: string, range: VSRange.t) =>
    overlayTextPrim(editor, VSCode.StringOr.string(color), text, range);

  let underlineText = (editor: TextEditor.t, range: VSRange.t) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.OpenOpen);
    let textDecoration = "underline dotted";
    let options =
      DecorationRenderOptions.t(~rangeBehavior, ~textDecoration, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, [|range|]);
    decoration;
  };

  let destroy = TextEditorDecorationType.dispose;
};

module Cursor = {
  let set = (editor, point) => {
    let selection = Selection.make(point, point);
    editor->TextEditor.setSelection(selection);
  };
  let setMany = (editor, points) => {
    let selections = points->Array.map(point => Selection.make(point, point));
    editor->TextEditor.setSelections(selections);
  };
  let get = editor => editor->TextEditor.selection->Selection.active;
  let getMany = editor =>
    editor->TextEditor.selections->Array.map(Selection.active);
};

module Selection = {
  let set = (editor, range) => {
    let selection =
      Selection.make(VSRange.start(range), VSRange.end_(range));
    editor->TextEditor.setSelection(selection);
  };
  let setMany = (editor, ranges) => {
    let selections =
      ranges->Array.map(range =>
        Selection.make(VSRange.start(range), VSRange.end_(range))
      );
    editor->TextEditor.setSelections(selections);
  };
  let get = editor => {
    let selection = editor->TextEditor.selection;
    VSRange.make(Selection.start(selection), Selection.end_(selection));
  };
  let getMany = editor => {
    editor
    ->TextEditor.selections
    ->Array.map(selection =>
        VSRange.make(Selection.start(selection), Selection.end_(selection))
      );
  };
};

module Text = {
  let get = (document, range) =>
    document->TextDocument.getText(Some(range));
  let getAll = document => document->TextDocument.getText(None);

  let replace = (document, range, text) => {
    let workspaceEdit = WorkspaceEdit.make();
    workspaceEdit->WorkspaceEdit.replace(
      document->TextDocument.uri,
      range,
      text,
      None,
    );
    Workspace.applyEdit(workspaceEdit);
  };
  let batchReplace = (document, replacements) => {
    let workspaceEdit = WorkspaceEdit.make();
    replacements->Array.forEach(((range, text)) =>
      workspaceEdit->WorkspaceEdit.replace(
        document->TextDocument.uri,
        range,
        text,
        None,
      )
    );
    Workspace.applyEdit(workspaceEdit);
  };

  let insert = (document, point, text) => {
    let workspaceEdit = WorkspaceEdit.make();
    workspaceEdit->WorkspaceEdit.insert(
      document->TextDocument.uri,
      point,
      text,
      None,
    );
    Workspace.applyEdit(workspaceEdit);
  };
  let batchInsert = (document, points, text) => {
    let workspaceEdit = WorkspaceEdit.make();
    let textEdits = points->Array.map(point => TextEdit.insert(point, text));
    workspaceEdit->WorkspaceEdit.set(document->TextDocument.uri, textEdits);
    Workspace.applyEdit(workspaceEdit);
  };
  let delete = (document, range) => {
    let workspaceEdit = WorkspaceEdit.make();
    workspaceEdit->WorkspaceEdit.delete(
      document->TextDocument.uri,
      range,
      None,
    );
    Workspace.applyEdit(workspaceEdit);
  };
};

// returns an array of UTF-16 based indices where surrogate pairs occur,
// for example, suppose that there are surrogate pairs at [6000, 6001] and [6003, 6004]
//
//        UTF-8       UTF-16
//        --------------------
//        5999        5999
//        6000        6000
//                    6001
//        6001        6002
//        6002        6003
//                    6004
//        6003        6005
//
// then this function should return [6000, 6003]
let computeUTF16SurrogatePairIndices = (text: string): array(int) => {
  let surrogatePairs = [||];

  // length in code units (16 bits), not the actual UTF-8 length
  let lengthInCodeUnits = String.length(text);

  // iterate through the text to find surrogate pairs
  let i = ref(0);
  while (i^ < lengthInCodeUnits) {
    let charCode = Js.String.charCodeAt(i^, text)->int_of_float;
    let notFinal = i^ + 1 < lengthInCodeUnits;
    // check if this is a part of a surrogate pair
    if (charCode >= 0xD800 && charCode <= 0xDBFF && notFinal) {
      // found the high surrogate, proceed to check the low surrogate
      let nextCharCode = Js.String.charCodeAt(i^ + 1, text)->int_of_float;
      if (nextCharCode >= 0xDC00 && charCode <= 0xDFFF) {
        // store the index of this surrogate pair
        Js.Array.push(i^, surrogatePairs)
        ->ignore;
      };
      // increment by 2 because we have checked the presumably low surrogate char
      i := i^ + 2;
    } else {
      i := i^ + 1;
    };
  };

  surrogatePairs;
};

module type Indices = {
  type t;
  let make: array(int) => t;
  let convert: (t, int) => int;
  // expose the intervals for testing
  let expose: t => (array((int, int)), int);
};

module Indices: Indices = {
  //    Problem:  Symbols like "𝕁" should be treated like a single character as in UTF-8,
  //              however, it's treated like 2 characters in UTF-16 (which is what VS Code uses)
  type t = {
    intervals: array((int, int)),
    mutable cursor: int,
  };

  // compile an array of UTF-8 based offset intervals
  // for faster UTF-8 => UTF-16 convertion
  let make = (indicesUTF16: array(int)): t => {
    //  Suppose that, there are surrogate pairs at [6000, 6001] and [6003, 6004]
    //
    //        UTF-8       UTF-16
    //        --------------------
    //        5999        5999
    //        6000        6000           <
    //                    6001
    //        6001        6002
    //        6002        6003           <
    //                    6004
    //        6003        6005
    //
    //  When converting from a UTF-8 based index, say, `6003`,
    //  we need to know how many surrogate pairs are there before `6003`
    //
    //  Here's what we have computed:
    //    * UTF-16 based indices of surrogate pairs: [6000, 6003]
    //
    //  Here's what we are going to compute next:
    //    * UTF-8 based indices of surrogate pairs: [6000, 6002]
    //    * intervals of UTF-8 based indices [(0, 6000), (6001, 6002)]
    //
    //  NOTE: the last interval (6003, ...) will not be included

    //  indicesUTF16 = [6000, 6003]

    // [6000, 6002]
    let indicesUTF8 = indicesUTF16->Array.mapWithIndex((i, x) => x - i);

    // [(0, 6000), (6001, 6002)]
    let intervals =
      indicesUTF8->Array.mapWithIndex((i, rightEndpoint) => {
        let leftEndpoint =
          switch (indicesUTF8[i - 1]) {
          | Some(x) => x + 1
          // first interval
          | None => 0
          };
        (leftEndpoint, rightEndpoint);
      });
    {intervals, cursor: 0};
  };

  let rec convert = (self, index) => {
    switch (self.intervals[self.cursor]) {
    | None =>
      // happens when we have passed the last inverval
      // return index + how many pairs it have skipped
      index + self.cursor
    | Some((left, right)) =>
      if (index < left) {
        // reset the cursor to the beginning of the intervals
        self.cursor = 0;
        convert(self, index);
      } else if (index > right) {
        // move the cursor a tad right
        self.cursor = self.cursor + 1;
        convert(self, index);
      } else {
        // index + how many pairs it have skipped
        index + self.cursor;
      }
    };
  };

  let expose = self => (self.intervals, self.cursor);
};

let toUTF8Offset = (document, offset) => {
  let range =
    VSRange.make(
      VSCode.Position.make(0, 0), // start
      document->TextDocument.positionAt(offset) // end
    );
  let text = document->TextDocument.getText(Some(range));
  Sig.characterWidth(text);
};

let focus = document => {
  Window.showTextDocument(document, ~column=ViewColumn.Beside, ())->ignore;
};

let reveal = (editor, range) => {
  editor->TextEditor.revealRange(
    range,
    Some(TextEditorRevealType.InCenterIfOutsideViewport),
  );
};

module Provider = {
  let registerProvider = (definitionProvider, hoverProvider) => {
    let documentSelector = [|VSCode.StringOr.string("agda")|];

    let definitionProvider =
      DefinitionProvider.{
        provideDefinition: (textDocument, point, _) => {
          definitionProvider(textDocument->TextDocument.fileName, point)
          ->ProviderResult.map(pairs =>
              LocationLinkOrLocation.locationLinks(
                pairs->Array.map(((srcRange, targetFile, targetPos)) =>
                  LocationLink.{
                    originSelectionRange: Some(srcRange),
                    targetRange: VSRange.make(targetPos, targetPos),
                    targetSelectionRange: None,
                    targetUri: Uri.file(targetFile),
                  }
                ),
              )
            );
        },
      };

    let hoverProvider =
      HoverProvider.{
        provideHover: (textDocument, point, _) =>
          hoverProvider(textDocument->TextDocument.fileName, point)
          ->ProviderResult.map(((strings, range)) => {
              let markdownStrings =
                strings->Belt.Array.map(string =>
                  MarkdownString.make(string, true)
                );
              Hover.makeWithRange(markdownStrings, range);
            }),
      };

    [|
      Languages.registerDefinitionProvider(
        documentSelector,
        definitionProvider,
      ),
      Languages.registerHoverProvider(documentSelector, hoverProvider),
    |];
  };

  module Mock = {
    // https://code.visualstudio.com/api/references/vscode-api#SemanticsTokens
    module SemanticsTokens = {
      type t;
      // constructors
      [@bs.module "vscode"] [@bs.new]
      external make: array(int) => t = "SemanticsTokens";
      [@bs.module "vscode"] [@bs.new]
      external makeWithResultId: (array(int), string) => t =
        "SemanticsTokens";
      // properties
      [@bs.get] external data: t => array(int) = "data";
      [@bs.get] external resultId: t => option(string) = "resultId";
    };

    // https://code.visualstudio.com/api/references/vscode-api#SemanticTokensLegend
    module SemanticTokensLegend = {
      type t;
      // constructors
      [@bs.module "vscode"] [@bs.new]
      external make: array(string) => t = "SemanticTokensLegend";
      [@bs.module "vscode"] [@bs.new]
      external makeWithTokenModifiers: (array(string), array(string)) => t =
        "SemanticTokensLegend";
      // properties
      [@bs.get]
      external tokenModifiers: t => array(string) = "tokenModifiers";
      [@bs.get] external tokenTypes: t => array(string) = "tokenTypes";
    };

    // https://code.visualstudio.com/api/references/vscode-api#SemanticTokensBuilder
    module SemanticTokensBuilder = {
      type t;
      // constructors
      [@bs.module "vscode"] [@bs.new]
      external make: unit => t = "SemanticTokensBuilder";
      [@bs.module "vscode"] [@bs.new]
      external makeWithLegend: SemanticTokensLegend.t => t =
        "SemanticTokensBuilder";
      // methods
      [@bs.send] external build: t => SemanticsTokens.t = "build";
      [@bs.send]
      external buildWithResultId: (t, string) => SemanticsTokens.t = "build";
      [@bs.send]
      external push: (t, int, int, int, int, option(int)) => unit = "push";
      [@bs.send]
      external pushLegend:
        (t, VSRange.t, string, option(array(string))) => unit =
        "push";
    };

    // https://code.visualstudio.com/api/references/vscode-api#DocumentSemanticTokensProvider
    module DocumentSemanticTokensProvider = {
      // missing: onDidChangeSemanticTokens
      // missing: provideDocumentSemanticTokensEdits
      type t = {
        provideDocumentSemanticTokens:
          (TextDocument.t, CancellationToken.t) =>
          ProviderResult.t(SemanticsTokens.t),
      };
    };

    module Languages = {
      [@bs.module "vscode"] [@bs.scope "languages"]
      external registerDocumentSemanticTokensProvider:
        (
          DocumentSelector.t,
          DocumentSemanticTokensProvider.t,
          SemanticTokensLegend.t
        ) =>
        Disposable.t =
        "registerDocumentSemanticTokensProvider";
    };
  };

  let registerTestingProvider = (prodider, (tokenTypes, tokenModifiers)) => {
    let documentSelector = [|VSCode.StringOr.string("agda")|];
    let semanticTokensLegend =
      Mock.SemanticTokensLegend.makeWithTokenModifiers(
        tokenTypes,
        tokenModifiers,
      );

    let documentSemanticTokensProvider =
      Mock.DocumentSemanticTokensProvider.{
        provideDocumentSemanticTokens: (textDocument, _) => {
          let builder =
            Mock.SemanticTokensBuilder.makeWithLegend(semanticTokensLegend);
          let pushLegend = Mock.SemanticTokensBuilder.pushLegend(builder);
          prodider(textDocument->TextDocument.fileName, pushLegend)
          ->ProviderResult.map(() =>
              Mock.SemanticTokensBuilder.build(builder)
            );
        },
      };

    [|
      Mock.Languages.registerDocumentSemanticTokensProvider(
        documentSelector,
        documentSemanticTokensProvider,
        semanticTokensLegend,
      ),
    |];
  };
};
