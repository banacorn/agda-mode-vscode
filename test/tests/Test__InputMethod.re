open VSCode;
module VSRange = Range;
open Belt;
open! BsMocha.Mocha;
open Test__Util;

module Js' = Js;
open Promise;
module Js = Js';

type setup = {
  editor: TextEditor.t,
  emitter: Event.t(EditorIM.event),
};

let activateExtension = (fileName): Promise.t(setup) => {
  let disposables = [||];
  let extensionPath = Path.extensionPath();
  let emitter = Main.activateWithoutContext(disposables, extensionPath);
  Window.showTextDocumentWithUri(Uri.file(fileName), None)
  ->map(editor => {editor, emitter});
};

let acquire = setup => {
  switch (setup^) {
  | None => resolved(Error(Util.Error("Cannot acquire the setup")))
  | Some(setup) => resolved(Ok(setup))
  };
};

let cleanup = setup => {
  let range = VSRange.make(Position.make(0, 0), Position.make(100, 0));
  setup.editor->TextEditor.document->Editor.Text.replace(range, "");
};

let insertChar = (setup, char) => {
  let promise = setup.emitter.once();
  let positions =
    setup.editor->TextEditor.selections->Array.map(Selection.end_);
  setup.editor
  ->TextEditor.document
  ->Editor.Text.batchInsert(positions, char)
  ->flatMap(_ => promise)
  ->map(x => Ok(x));
};

let backspace = setup => {
  let promise = setup.emitter.once();
  let end_ = setup.editor->TextEditor.selection->Selection.end_;
  let start = end_->Position.translate(0, -1);
  let range = VSRange.make(start, end_);
  setup.editor
  ->TextEditor.document
  ->Editor.Text.delete(range)
  ->flatMap(_ => promise)
  ->map(x => Ok(x));
};

module IM = {
  let activate = (setup, ~positions=?, ()) => {
    let promise = setup.emitter.once();
    let positions =
      positions->Option.getWithDefault([|
        setup.editor->TextEditor.selection->Selection.end_,
      |]);
    setup.editor
    ->TextEditor.setSelections(
        positions->Array.map(point => Selection.make(point, point)),
      );
    VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
    ->flatMap(result => result)
    ->flatMap(_ => promise)
    ->map(x => Ok(x));
    // document->Editor.insertTexts(points, "\\")->flatMap(_ => promise);
  };

  let deactivate = setup => {
    let promise = setup.emitter.once();
    VSCode.Commands.executeCommand0("agda-mode.escape")
    ->flatMap(result => result)
    ->flatMap(_ => promise)
    ->map(x => Ok(x));
  };
};

describe("Input Method (Editor)", () => {
  let setup = ref(None);

  Q.before(() => {
    activateExtension(Path.asset("InputMethod.agda"))
    ->map(value => {
        setup := Some(value);
        Ok();
      })
  });

  Q.after_each(() => {acquire(setup)->mapOk(cleanup)});

  describe("Insertion", () => {
    Q.it({j|should translate "lambdabar" to "λ"|j}, () => {
      acquire(setup)
      ->flatMapOk(setup => {
          let document = TextEditor.document(setup.editor);
          IM.activate(setup, ())
          ->flatMapOk(A.equal(EditorIM.Activate))
          ->flatMapOk(() => insertChar(setup, "l"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() => A.equal({j|←|j}, Editor.Text.getAll(document)))
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal({j|←a|j}, Editor.Text.getAll(document))
            )
          ->flatMapOk(() => insertChar(setup, "m"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal({j|←am|j}, Editor.Text.getAll(document))
            )
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal({j|←amb|j}, Editor.Text.getAll(document))
            )
          ->flatMapOk(() => insertChar(setup, "d"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal({j|←ambd|j}, Editor.Text.getAll(document))
            )
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() => A.equal({j|λ|j}, Editor.Text.getAll(document)))
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() => A.equal({j|λb|j}, Editor.Text.getAll(document)))
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal({j|λba|j}, Editor.Text.getAll(document))
            )
          ->flatMapOk(() => insertChar(setup, "r"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() => A.equal({j|ƛ|j}, Editor.Text.getAll(document)));
        })
    });
    Q.it({j|should translate "bn" to "𝕟"|j}, () => {
      acquire(setup)
      ->flatMapOk(setup => {
          let document = TextEditor.document(setup.editor);
          IM.activate(setup, ())
          ->flatMapOk(A.equal(EditorIM.Activate))
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() => {
              A.equal({j|♭|j}, Editor.Text.getAll(document))
            })
          ->flatMapOk(() => insertChar(setup, "n"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() => {
              A.equal({j|𝕟|j}, Editor.Text.getAll(document))
            });
          // ->flatMapOk(() => IM.deactivate(setup))
          // ->flatMapOk(A.equal(EditorIM.Deactivate))
        })
    });
  });
  describe("Backspacing", () => {
    Q.it({j|should work just fine|j}, () => {
      acquire(setup)
      ->flatMapOk(setup => {
          let document = TextEditor.document(setup.editor);
          IM.activate(setup, ())
          ->flatMapOk(A.equal(EditorIM.Activate))
          ->flatMapOk(() => insertChar(setup, "l"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() => A.equal({j|←|j}, Editor.Text.getAll(document)))
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal({j|←a|j}, Editor.Text.getAll(document))
            )
          ->flatMapOk(() => insertChar(setup, "m"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal({j|←am|j}, Editor.Text.getAll(document))
            )
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal({j|←amb|j}, Editor.Text.getAll(document))
            )
          ->flatMapOk(() => insertChar(setup, "d"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal({j|←ambd|j}, Editor.Text.getAll(document))
            )
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() => A.equal({j|λ|j}, Editor.Text.getAll(document)))
          ->flatMapOk(() => backspace(setup))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() => {
              A.equal({j|lambd|j}, Editor.Text.getAll(document))
            })
          ->flatMapOk(() => IM.deactivate(setup))
          ->flatMapOk(A.equal(EditorIM.Deactivate));
        })
    })
  });
  describe("Multiple cursors at once", () => {
    let positions = [|
      Position.make(0, 0),
      Position.make(1, 0),
      Position.make(2, 0),
      Position.make(3, 0),
    |];
    Q.it({j|should work just fine|j}, () => {
      let replaceCRLF = Js.String.replaceByRe([%re "/\\r\\n/g"], "\n");

      acquire(setup)
      ->flatMapOk(setup => {
          let document = TextEditor.document(setup.editor);

          document
          ->Editor.Text.insert(Position.make(0, 0), "\n\n\n")
          ->flatMap(_ => IM.activate(setup, ~positions, ()))
          ->flatMapOk(A.equal(EditorIM.Activate))
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal(
                {j|♭\n♭\n♭\n♭|j},
                replaceCRLF(Editor.Text.getAll(document)),
              )
            )
          ->flatMapOk(() => insertChar(setup, "n"))
          ->flatMapOk(A.equal(EditorIM.Change))
          ->flatMapOk(() =>
              A.equal(
                {j|𝕟\n𝕟\n𝕟\n𝕟|j},
                replaceCRLF(Editor.Text.getAll(document)),
              )
            );
        });
    });
  });
});
