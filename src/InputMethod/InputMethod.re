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
    // mutable updates: array((int, int, string, Instance.t)),
    mutable cursorsToBeChecked: option(array(Editor.Point.t)),
    mutable lock: bool,
  };

  let insertBackslash = editor => {
    Editor.getCursorPositions(editor)
    ->Array.forEach(point => {
        Editor.insertText(editor, point, "\\")->ignore
      });
  };

  let activate = (self, editor, offsets: array(int)) => {
    Js.log("locked: " ++ string_of_bool(self.lock));
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
    let cursorChangelistener = (points: array(Editor.Point.t)) => {
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

    let iterateThroughUpdates = updates => {
      let rec go:
        (int, list((int, int, string, Instance.t))) => Promise.t(unit) =
        accum =>
          fun
          | [] => Promise.resolved()
          | [(start, end_, text, instance), ...updates] => {
              let range =
                Editor.Range.make(
                  Editor.pointAtOffset(editor, start + accum),
                  Editor.pointAtOffset(editor, end_ + accum),
                );

              // this value represents the offset change made by this update
              // e.g. "lambda" => "λ" would result in `-5`
              let delta = String.length(text) - (end_ - start);

              Js.log(
                "!!! "
                ++ text
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
              Editor.setText(editor, range, text)
              ->Promise.flatMap(_ => go(accum, updates));
            };

      Js.log("!!! UPDATES start " ++ string_of_int(Array.length(updates)));

      go(0, List.fromArray(updates))
      ->Promise.tap(() => {
          Js.log(
            "!!! UPDATES finish " ++ string_of_int(Array.length(updates)),
          )
        });
    };

    // update offsets of Instances base on changeEvents
    let updateOffsets = (changes: array(Editor.changeEvent)) => {
      Js.log("~~~ offset start");
      // sort the changes base on their offsets in the ascending order
      let changes =
        Js.Array.sortInPlaceWith(
          (x: Editor.changeEvent, y: Editor.changeEvent) =>
            compare(x.offset, y.offset),
          changes,
        );

      let updates = [||];

      // iterate through changeEvents
      // and push updates to the queue
      let rec scanAndUpdate:
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
                    (accum + start, accum + end_ + delta, text, instance),
                    updates,
                  )
                  ->ignore;
                  instance.buffer = buffer;
                };

                instance.range = (accum + start, accum + end_ + delta);
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

      Js.log("~~~ offset finish");

      (
        if (Array.length(updates) > 0) {
          iterateThroughUpdates(updates);
        } else {
          Promise.resolved();
        }
      )
      ->Promise.get(() => {
          self.lock = false;
          Js.log(">>>>>>>>>>>>>>>>> UNLOCK");
          switch (self.cursorsToBeChecked) {
          | None => ()
          | Some(points) =>
            self.cursorsToBeChecked = None;
            cursorChangelistener(points);
          };
        });
    };

    // initiate listeners
    cursorChangeHandle :=
      Some(
        Editor.onChangeCursorPosition(points => {
          self.cursorsToBeChecked = Some(points);
          Js.log("locked2: " ++ string_of_bool(self.lock));
          if (self.lock) {
            Js.log("### wait");
          } else {
            cursorChangelistener(points);
          };
        }),
      );
    editorChangeHandle :=
      Some(
        Editor.onChange(changes
          // editing happened
          =>
            if (!self.lock && Array.length(changes) > 0) {
              self.lock = true;
              checkIfEveryoneIsStillAlive();
              Js.log(">>>>>>>>>>>>>>>>> LOCK");
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
    lock: false,
  };
};