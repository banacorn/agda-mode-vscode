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
    mutable cursorsToBeChecked: option(array(Editor.Point.t)),
    mutable busy: bool,
  };

  // datatype for representing a rewrite to be made to the text editor
  type rewrite = {
    start: int,
    end_: int,
    rewriteWith: string,
    instance: Instance.t,
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
      Js.Array.sortInPlaceWith(compare, offsets)
      ->Array.map(Instance.make(editor));

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

    // kill the Instances that are not are not pointed by cursors
    let validate = (points: array(Editor.Point.t)) => {
      let offsets = points->Array.map(Editor.offsetAtPoint(editor));
      Js.log(
        "\n### Cursors  : "
        ++ Js.Array.sortInPlaceWith(compare, offsets)
           ->Array.map(string_of_int)
           ->Util.Pretty.array
        ++ "\n### Instances: "
        ++ self.instances
           ->Array.map(i =>
               "("
               ++ string_of_int(fst(i.range))
               ++ ", "
               ++ string_of_int(snd(i.range))
               ++ ")"
             )
           ->Util.Pretty.array,
      );
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

      self.cursorsToBeChecked = None;
      checkIfEveryoneIsStillAlive();
    };

    // iterate through a list of rewrites and apply them to the text editor
    let applyRewrite = rewrites => {
      let rec go: (int, list(rewrite)) => Promise.t(unit) =
        accum =>
          fun
          | [] => Promise.resolved()
          | [{start, end_, rewriteWith, instance}, ...rewrites] => {
              let range =
                Editor.Range.make(
                  Editor.pointAtOffset(editor, start + accum),
                  Editor.pointAtOffset(editor, end_ + accum),
                );

              // this value represents the offset change made by this update
              // e.g. "lambda" => "λ" would result in `-5`
              let delta = String.length(rewriteWith) - (end_ - start);

              Js.log(
                "!!! "
                ++ rewriteWith
                ++ " ("
                ++ string_of_int(accum + start)
                ++ ","
                ++ string_of_int(accum + end_)
                ++ ") => ("
                ++ string_of_int(accum + start)
                ++ ","
                ++ string_of_int(accum + end_ + delta)
                ++ ") "
                ++ string_of_int(accum),
              );

              // Js.log(instance.range);
              instance.range = (accum + start, accum + end_ + delta);
              // pass this change down
              let accum = accum + delta;

              // update the text buffer
              Editor.setText(editor, range, rewriteWith)
              ->Promise.flatMap(_ => go(accum, rewrites));
            };

      go(0, List.fromArray(rewrites));
    };

    // update offsets of Instances base on changeEvents
    let updateOffsets = (changes: array(Editor.changeEvent)) => {
      // sort the changes base on their offsets in the ascending order
      let changes =
        Js.Array.sortInPlaceWith(
          (x: Editor.changeEvent, y: Editor.changeEvent) =>
            compare(x.offset, y.offset),
          changes,
        );

      let rewrites = [||];

      // iterate through changeEvents
      // and push rewrites to the queue
      let rec go:
        (int, (list(Editor.changeEvent), list(Instance.t))) =>
        list(Instance.t) =
        accum =>
          fun
          | ([change, ...cs], [instance, ...is]) => {
              let (start, end_) = instance.range;
              let delta =
                String.length(change.insertText) - change.replaceLength;
              // `change.offset` is the untouched offset before any modifications happened
              // so it's okay to compare it with the also untouched `instance.range`
              if (Instance.withIn(instance, change.offset)) {
                // `change` appears inside the `instance`
                let next =
                  Buffer2.update(
                    fst(instance.range),
                    instance.buffer,
                    change,
                  );
                switch (next) {
                | Noop => ()
                | UpdateAndReplaceText(buffer, text) =>
                  Js.Array.push(
                    {
                      start: accum + start,
                      end_: accum + end_ + delta,
                      rewriteWith: text,
                      instance,
                    },
                    rewrites,
                  )
                  ->ignore;
                  instance.buffer = buffer;
                };

                instance.range = (accum + start, accum + end_ + delta);
                [instance, ...go(accum + delta, (cs, is))];
              } else if (change.offset < fst(instance.range)) {
                // `change` appears before the `instance`
                go(
                  accum + delta, // update only `accum`
                  (cs, [instance, ...is]),
                );
              } else {
                // `change` appears after the `instance`
                instance.range = (accum + start, accum + end_);
                [instance, ...go(accum, ([change, ...cs], is))];
              };
            }
          | ([], [instance, ...is]) => [instance, ...is]
          | (_, []) => [];

      // store the updated instances
      self.instances =
        go(0, (List.fromArray(changes), List.fromArray(self.instances)))
        ->List.toArray;

      // see if there are any rewrites to be done
      (
        if (Array.length(rewrites) > 0) {
          applyRewrite(rewrites);
        } else {
          Promise.resolved();
        }
      )
      ->Promise.get(() => {
          // all offsets updated and rewrites applied, reset the semaphore
          self.busy = false;
          // see if there are any pending validations
          switch (self.cursorsToBeChecked) {
          | None => ()
          | Some(points) => validate(points)
          };
        });
    };

    // initiate listeners
    cursorChangeHandle :=
      Some(
        Editor.onChangeCursorPosition(points =>
          if (self.busy) {
            // cannot validate cursors at this moment
            // store the positions and wait until the system is not busy
            self.cursorsToBeChecked =
              Some(points);
          } else {
            validate(points);
          }
        ),
      );
    editorChangeHandle :=
      Some(
        Editor.onChange(changes =>
          if (!self.busy && Array.length(changes) > 0) {
            // if any changes occured to the editor and the system is not busy
            self.busy = true;
            // update the offsets to reflect the changes
            updateOffsets(changes);
          }
        ),
      );
  };

  let make = () => {
    onAction: Event.make(),
    instances: [||],
    activated: false,
    cursorsToBeChecked: None,
    busy: false,
  };
};