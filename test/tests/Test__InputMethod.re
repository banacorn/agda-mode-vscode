open! BsMocha.Mocha;
open! Belt;

open Test__Util;
module Goal = Goal.Impl(Editor);
module Task = Task.Impl(Editor);
module InputMethod = InputMethod.Impl(Editor);
module Dispatcher = Dispatcher.Impl(Editor);
module GoalHandler = Handle__Goal.Impl(Editor);

module Console = Js.Console;
module Exn = Js.Exn;
module JsPromise = Js.Promise;
open Promise;

type setup = {
  editor: Editor.editor,
  emitter: Event.t(InputMethod.event),
};

let activateExtension = (fileName): Promise.t(setup) => {
  module Main = Main.Impl(Editor);

  let disposables = [||];
  let extensionPath = Path.extensionPath();
  let emitter = Main.activateWithoutContext(disposables, extensionPath);
  openTextEditor(fileName)->map(editor => {editor, emitter});
};

let acquire = setup => {
  switch (setup^) {
  | None => resolved(Error(Util.Error("Cannot acquire the setup")))
  | Some(setup) => resolved(Ok(setup))
  };
};

let cleanup = setup => {
  let range =
    Editor.Range.make(Editor.Point.make(0, 0), Editor.Point.make(100, 0));
  setup.editor->Editor.replaceText(range, "")->map(_ => Ok());
};

let insertChar = (setup, char) => {
  let promise = setup.emitter.once();
  let positions = Editor.getCursorPositions(setup.editor);
  setup.editor
  ->Editor.insertTexts(positions, char)
  ->flatMap(_ => promise)
  ->map(x => Ok(x));
};

let backspace = setup => {
  let promise = setup.emitter.once();
  let end_ = Editor.getCursorPosition(setup.editor);
  let start = end_->Editor.Point.translate(0, -1);
  let range = Editor.Range.make(start, end_);
  setup.editor
  ->Editor.deleteText(range)
  ->flatMap(_ => promise)
  ->map(x => Ok(x));
};

module IM = {
  let activate' = () => {
    VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
    ->flatMap(result => result)
    ->map(x => Ok(x));
  };

  let activate = (setup, ~positions=?, ()) => {
    let promise = setup.emitter.once();
    let positions =
      positions->Option.getWithDefault([|
        Editor.getCursorPosition(setup.editor),
      |]);
    setup.editor->Editor.setCursorPositions(positions);
    VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
    ->flatMap(result => result)
    ->flatMap(_ => promise)
    ->map(x => Ok(x));
    // setup.editor->Editor.insertTexts(points, "\\")->flatMap(_ => promise);
  };

  let deactivate = setup => {
    let promise = setup.emitter.once();
    VSCode.Commands.executeCommand0("agda-mode.escape")
    ->flatMap(result => result)
    ->flatMap(_ => promise)
    ->map(x => Ok(x));
  };
};

describe_only("InputMethod", () => {
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
          IM.activate(setup, ())
          ->flatMapOk(A.equal(InputMethod.Activate))
          ->flatMapOk(() => insertChar(setup, "l"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() => A.equal({j|←|j}, Editor.getText(setup.editor)))
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|←a|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "m"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|←am|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|←amb|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "d"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|←ambd|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() => A.equal({j|λ|j}, Editor.getText(setup.editor)))
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() => A.equal({j|λb|j}, Editor.getText(setup.editor)))
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|λba|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "r"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() => A.equal({j|ƛ|j}, Editor.getText(setup.editor)))
        })
    });
    Q.it({j|should translate "bn" to "𝕟"|j}, () => {
      acquire(setup)
      ->flatMapOk(setup => {
          IM.activate(setup, ())
          ->flatMapOk(A.equal(InputMethod.Activate))
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() => {
              A.equal({j|♭|j}, Editor.getText(setup.editor))
            })
          ->flatMapOk(() => insertChar(setup, "n"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() => {
              A.equal({j|𝕟|j}, Editor.getText(setup.editor))
            })
          // ->flatMapOk(() => IM.deactivate(setup))
          // ->flatMapOk(A.equal(InputMethod.Deactivate))
        })
    });
  });
  describe("Backspace", () => {
    Q.it({j|should work just fine|j}, () => {
      acquire(setup)
      ->flatMapOk(setup => {
          IM.activate(setup, ())
          ->flatMapOk(A.equal(InputMethod.Activate))
          ->flatMapOk(() => insertChar(setup, "l"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() => A.equal({j|←|j}, Editor.getText(setup.editor)))
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|←a|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "m"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|←am|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|←amb|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "d"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|←ambd|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "a"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() => A.equal({j|λ|j}, Editor.getText(setup.editor)))
          ->flatMapOk(() => backspace(setup))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() => {
              A.equal({j|lambd|j}, Editor.getText(setup.editor))
            })
          ->flatMapOk(() => IM.deactivate(setup))
          ->flatMapOk(A.equal(InputMethod.Deactivate))
        })
    })
  });
  describe("Multiple cursors at once", () => {
    let positions = [|
      Editor.Point.make(0, 0),
      Editor.Point.make(1, 0),
      Editor.Point.make(2, 0),
      Editor.Point.make(3, 0),
    |];
    Q.it({j|should work just fine|j}, () => {
      acquire(setup)
      ->flatMapOk(setup => {
          setup.editor
          ->Editor.insertText(Editor.Point.make(0, 0), "\n\n\n")
          ->flatMap(_ => IM.activate(setup, ~positions, ()))
          ->flatMapOk(A.equal(InputMethod.Activate))
          ->flatMapOk(() => insertChar(setup, "b"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal({j|♭\n♭\n♭\n♭|j}, Editor.getText(setup.editor))
            )
          ->flatMapOk(() => insertChar(setup, "n"))
          ->flatMapOk(A.equal(InputMethod.Change))
          ->flatMapOk(() =>
              A.equal(
                {j|𝕟\n𝕟\n𝕟\n𝕟|j},
                Editor.getText(setup.editor),
              )
            )
          // ->tapOk(() => Editor.getText(setup.editor)->Console.info)
          // ->flatMapOk(() => IM.deactivate(setup))
          // ->flatMapOk(A.equal(InputMethod.Deactivate))
        })
    });
  });
});
