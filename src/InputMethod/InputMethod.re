open Belt;

module Impl = (Editor: Sig.Editor) => {
  open Command.InputMethodAction;
  module Buffer2 = Buffer2.Impl(Editor);

  module Instance = {
    type t = {
      mutable range: (int, int),
      decoration: array(Editor.Decoration.t),
      mutable buffer: Buffer2.t,
    };

    let make = (editor, offset) => {
      let point = Editor.pointAtOffset(editor, offset);
      {
        range: (offset, offset),
        decoration:
          Editor.Decoration.underlineText(
            editor,
            Editor.Range.make(point, point),
          ),
        buffer: Buffer2.make(),
      };
    };

    let withIn = (instance, offset) => {
      let (start, end_) = instance.range;
      start <= offset && offset <= end_;
    };

    let destroy = instance => {
      Js.log("KILLED");
      instance.decoration->Array.forEach(Editor.Decoration.destroy);
    };
  };

  type t = {
    onAction: Event.t(Command.InputMethodAction.t),
    mutable instances: array(Instance.t),
    mutable activated: bool,
  };

  let insertBackslash = editor => {
    Editor.getCursorPositions(editor)
    ->Array.forEach(point => {
        Editor.insertText(editor, point, "\\")->ignore
      });
  };

  let activate = (self, editor, offsets: array(int)) => {
    // instantiate from an array of offsets
    self.instances =
      offsets->Js.Array.sortInPlace->Array.map(Instance.make(editor));

    // handles of listeners
    let editorChangeHandle = ref(None);
    let cursorChangeHandle = ref(None);

    // destroy the handles if all Instances are destroyed
    let checkIfEveryoneIsStillAlive = () =>
      if (Array.length(self.instances) == 0) {
        Js.log("ALL DEAD");
        self.onAction.emit(Deactivate);
        (editorChangeHandle^)->Option.forEach(Editor.Disposable.dispose);
        (cursorChangeHandle^)->Option.forEach(Editor.Disposable.dispose);
      };

    // listeners
    let editorChangelistener = (changes: array(Editor.changeEvent)) => {
      // sort the changes base on their offsets, from small to big
      let changes =
        Js.Array.sortInPlaceWith(
          (x: Editor.changeEvent, y: Editor.changeEvent) =>
            compare(x.offset, y.offset),
          changes,
        );

      let rec scanAndUpdate:
        (int, (list(Editor.changeEvent), list(Instance.t))) =>
        list(Instance.t) =
        accum =>
          fun
          | ([change, ...cs], [instance, ...is]) => {
              let (start, end_) = instance.range;
              let delta =
                String.length(change.insertText) - change.replaceLength;

              let originalRange =
                Editor.Range.make(
                  Editor.pointAtOffset(editor, accum + start),
                  Editor.pointAtOffset(editor, accum + end_ + delta),
                );

              if (Instance.withIn(instance, change.offset)) {
                Js.log(instance.range);
                let next =
                  Buffer2.update(instance.range, instance.buffer, change);
                switch (next) {
                | Noop => ()
                | Update(buffer, range) =>
                  instance.buffer = buffer;
                  instance.range = range;
                | UpdateAndReplaceText(buffer, range, text) =>
                  instance.buffer = buffer;
                  instance.range = range;
                  Editor.setText(editor, originalRange, text)->ignore;
                  ();
                };

                // if (change.insertText != "" && change.replaceLength == 0) {
                //   Js.log("INSERT [" ++ change.insertText ++ "]");
                //   let translation = Translator.translate(change.insertText);
                //   switch (translation.symbol) {
                //   | None => ()
                //   | Some(symbol) =>
                //     let range =
                //       Editor.Range.make(
                //         Editor.pointAtOffset(editor, accum + start),
                //         Editor.pointAtOffset(editor, accum + end_ + delta),
                //       );
                //     Editor.setText(editor, range, symbol)->ignore;
                //   };
                //   Js.log(translation);
                //   ();
                // };

                // `change` appears inside the `instance`
                instance.range = (accum + start, accum + end_ + delta);

                // let range =
                //   Editor.Range.make(
                //     Editor.pointAtOffset(editor, accum + start),
                //     Editor.pointAtOffset(editor, accum + end_ + delta),
                //   );
                // let reality = Editor.getTextInRange(editor, range);
                // Js.log(Buffer.toString(instance.buffer));
                // switch (Buffer.next(instance.buffer, reality)) {
                // | Noop => ()
                // | Insert(buffer) =>
                //   instance.buffer = buffer;
                //   Js.log("INSERT! " ++ Buffer.toString(buffer));
                // | Backspace(buffer) =>
                //   instance.buffer = buffer;
                //   Js.log(Buffer.toString(buffer));
                // | Rewrite(buffer) =>
                //   instance.buffer = buffer;
                //   Js.log("Rewrite! " ++ Buffer.toString(buffer));
                // | Complete => ()
                // | Stuck(n) => ()
                // };

                [instance, ...scanAndUpdate(accum + delta, (cs, is))];
              } else if (change.offset < fst(instance.range)) {
                // `change` appears before the `instance`
                scanAndUpdate(
                  accum + delta, // update only `accum`
                  (cs, [instance, ...is]),
                );
              } else {
                // `change` appears after the `instance`
                instance.range = (accum + start, accum + end_);
                [instance, ...scanAndUpdate(accum, ([change, ...cs], is))];
              };
            }
          | ([], [instance, ...is]) => [instance, ...is]
          | (_, []) => [];
      self.instances =
        scanAndUpdate(
          0,
          (List.fromArray(changes), List.fromArray(self.instances)),
        )
        ->List.toArray;
    };

    // kill the Instances that are not are not pointed by cursors
    let cursorChangelistener = (points: array(Editor.Point.t)) => {
      let offsets = points->Array.map(Editor.offsetAtPoint(editor));
      self.instances =
        self.instances
        ->Array.keep((instance: Instance.t) => {
            // if any cursor falls into the range of the instance, the instance survives
            let survived =
              offsets->Belt.Array.some(Instance.withIn(instance));
            // if not, the instance gets destroyed
            if (!survived) {
              Instance.destroy(instance);
            };
            survived;
          });

      checkIfEveryoneIsStillAlive();
    };
    // initiate listeners
    editorChangeHandle :=
      Some(
        Editor.onChange(changes => {
          checkIfEveryoneIsStillAlive();
          editorChangelistener(changes);
        }),
      );
    cursorChangeHandle :=
      Some(Editor.onChangeCursorPosition(cursorChangelistener));
  };

  let make = () => {
    {onAction: Event.make(), instances: [||], activated: false};
  };
};