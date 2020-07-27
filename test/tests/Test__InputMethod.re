open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
// open VSCode;
open! Belt;

open Test__Util;
module Goal = Goal.Impl(Editor);
module Task = Task.Impl(Editor);
module InputMethod = InputMethod.Impl(Editor);
module Dispatcher = Dispatcher.Impl(Editor);
module GoalHandler = Handle__Goal.Impl(Editor);

let activateExtension =
    (fileName)
    : Promise.t(option((VSCode.TextEditor.t, Event.t(InputMethod.event)))) =>
  switch (VSCode.Extensions.getExtension("banacorn.agda-mode")) {
  | None =>
    BsMocha.Assert.fail("cannot acquire the extension");
    Promise.resolved(None);
  | Some(extension) =>
    openTextEditor(fileName)
    ->Promise.flatMap(editor => {
        extension
        ->VSCode.Extension.activate
        ->Promise.map(emitter => {
            module Console = Js.Console;
            Console.error("editor");
            Console.error(editor);
            Console.error("emitter");
            Console.error(emitter);
            Some((editor, emitter));
          })
      })
  };

let activateExtension2 =
    (fileName)
    : Promise.t(option((VSCode.TextEditor.t, Event.t(InputMethod.event)))) =>
  switch (VSCode.Extensions.getExtension("banacorn.agda-mode")) {
  | None =>
    BsMocha.Assert.fail("cannot acquire the extension");
    Promise.resolved(None);
  | Some(extension) =>
    openTextEditor(fileName)
    ->Promise.flatMap(editor => {
        extension
        ->VSCode.Extension.activate
        ->Promise.map(emitter => {
            module Console = Js.Console;
            Console.error("editor");
            Console.error(editor);
            Console.error("emitter");
            Console.error(emitter);
            Some((editor, emitter));
          })
      })
  };

let cleanup = editor => {
  let range =
    Editor.Range.make(Editor.Point.make(0, 0), Editor.Point.make(100, 0));
  editor->Editor.replaceText(range, "")->Promise.map(_ => ());
};

let insertChar =
    (emitter: Event.t(InputMethod.event), editor, ~positions=?, char) => {
  let promise = emitter.once();
  switch (positions) {
  | None =>
    let pos = Editor.getCursorPosition(editor);
    editor->Editor.insertText(pos, char)->Promise.flatMap(_ => promise);
  | Some(positions) =>
    editor->Editor.insertTexts(positions, char)->Promise.flatMap(_ => promise)
  };
};

let backspace = (emitter: Event.t(InputMethod.event), editor) => {
  let promise = emitter.once();
  let end_ = Editor.getCursorPosition(editor);
  let start = end_->Editor.Point.translate(0, -1);
  let range = Editor.Range.make(start, end_);
  editor->Editor.deleteText(range)->Promise.flatMap(_ => promise);
};

let activateInputhMethod = () => {
  VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
  ->Promise.flatMap(result => result);
};

let deactivateInputhMethod = () =>
  VSCode.Commands.executeCommand0("agda-mode.escape")
  ->Promise.flatMap(result => result);

describe_only("InputMethod", () => {
  let env = ref(None);
  Q.before(() => {
    activateExtension(Path.asset("InputMethod.agda"))
    ->Promise.map(
        fun
        | None =>
          BsMocha.Assert.fail(
            "cannot activate the extension and open an editor from \""
            ++ Path.asset("InputMethod.agda")
            ++ "\"",
          )
        | Some((editor, emitter)) => {
            env := Some((editor, emitter));
          },
      )
  });

  Q.after_each(() => {
    (env^)
    ->Option.mapWithDefault(Promise.resolved(), ((editor, _)) =>
        cleanup(editor)
      )
  });

  let acquire = env => {
    switch (env^) {
    | None => Promise.resolved(Error("Cannot acquire the resource"))
    | Some(env) => Promise.resolved(Ok(env))
    };
  };

  Promise.(
    describe("Insertion", () => {
      Q.it_only({j|should translate "lambda" to "λ"|j}, ()
        => {
          acquire(env)
          ->flatMapOk(((editor, emitter)) => {
              activateInputhMethod()
              ->flatMap(() => insertChar(emitter, editor, "l"))
              ->map(A.equal(InputMethod.Change))
              ->tap(() => Assert.equal(Editor.getText(editor), {j|←|j}))
              ->flatMap(_ => insertChar(emitter, editor, "a"))
              ->map(A.equal(InputMethod.Change))
              ->map(() => Assert.equal(Editor.getText(editor), {j|←a|j}))
              ->flatMap(() => insertChar(emitter, editor, "m"))
              ->map(A.equal(InputMethod.Change))
              ->map(() => Assert.equal(Editor.getText(editor), {j|←am|j}))
              ->flatMap(() => insertChar(emitter, editor, "b"))
              ->map(A.equal(InputMethod.Change))
              ->tap(() => Assert.equal(Editor.getText(editor), {j|←amb|j}))
              ->flatMap(() => insertChar(emitter, editor, "d"))
              ->map(A.equal(InputMethod.Change))
              ->tap(() =>
                  Assert.equal(Editor.getText(editor), {j|←ambd|j})
                )
              ->flatMap(() => insertChar(emitter, editor, "a"))
              ->map(A.equal(InputMethod.Change))
              ->tap(() => Assert.equal(Editor.getText(editor), {j|λ|j}))
              ->flatMap(deactivateInputhMethod)
              ->map(x => Ok(x))
            })
        })
        // Q.it({j|should translate "bn" to "𝕟"|j}, () => {
        //   acquire(env)
        //   ->flatMapOk(((editor, emitter)) => {
        //       activateInputhMethod()
        //       ->flatMap(() => insertChar(emitter, editor, "b"))
        //       ->map(A.equal(InputMethod.Change))
        //       ->flatMap(() => insertChar(emitter, editor, "n"))
        //       ->map(A.equal(InputMethod.Change))
        //       ->map(() => {Assert.equal(Editor.getText(editor), {j|𝕟|j})})
        //       ->flatMap(deactivateInputhMethod)
        //       ->map(x => Ok(x))
        //     })
        // });
    })
  );
  // describe("Backspace", () => {
  //   Q.it({j|should work just fine|j}, () => {
  //     acquire(env)
  //     ->flatMapOk(((editor, emitter)) => {
  //         activateInputhMethod()
  //         ->flatMap(() => insertChar(emitter, editor, "l"))
  //         ->map(A.equal(InputMethod.Change))
  //         ->tap(() => Assert.equal(Editor.getText(editor), {j|←|j}))
  //         ->flatMap(() => insertChar(emitter, editor, "a"))
  //         ->map(A.equal(InputMethod.Change))
  //         ->tap(() => Assert.equal(Editor.getText(editor), {j|←a|j}))
  //         ->flatMap(() => insertChar(emitter, editor, "m"))
  //         ->map(A.equal(InputMethod.Change))
  //         ->tap(() => Assert.equal(Editor.getText(editor), {j|←am|j}))
  //         ->flatMap(() => insertChar(emitter, editor, "b"))
  //         ->map(A.equal(InputMethod.Change))
  //         ->tap(() => Assert.equal(Editor.getText(editor), {j|←amb|j}))
  //         ->flatMap(() => insertChar(emitter, editor, "d"))
  //         ->map(A.equal(InputMethod.Change))
  //         ->tap(() => Assert.equal(Editor.getText(editor), {j|←ambd|j}))
  //         ->flatMap(() => insertChar(emitter, editor, "a"))
  //         ->map(A.equal(InputMethod.Change))
  //         ->tap(() => Assert.equal(Editor.getText(editor), {j|λ|j}))
  //         ->flatMap(() => backspace(emitter, editor))
  //         ->map(A.equal(InputMethod.Change))
  //         ->tap(() => Assert.equal(Editor.getText(editor), {j|lambd|j}))
  //         ->flatMap(deactivateInputhMethod)
  //         ->map(x => Ok(x))
  //       })
  //   })
  // });
});

// describe("Multiple cursors at once", () => {
//   let positions = [|
//     Editor.Point.make(0, 0),
//     Editor.Point.make(1, 0),
//     Editor.Point.make(2, 0),
//     Editor.Point.make(3, 0),
//   |];
//   Q.it({j|should work just fine|j}, () => {
//     (env^)
//     ->Option.mapWithDefault(Promise.resolved(), ((editor, emitter)) =>
//         activateInputhMethod()
//         ->Promise.flatMap(() => insertChar(emitter, editor, "\n\n\n"))
//         ->Promise.tap(() =>
//             Assert.equal(Editor.getText(editor), {j|←|j})
//           )
//         ->Promise.flatMap(() =>
//             insertChar(emitter, editor, ~positions, "l")
//           )
//         ->Promise.tap(() =>
//             Assert.equal(Editor.getText(editor), {j|←|j})
//           )
//         ->Promise.flatMap(deactivateInputhMethod)
//       )
//   });
// });
