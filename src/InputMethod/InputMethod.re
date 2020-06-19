open Belt;

module Impl = (Editor: Sig.Editor) => {
  open Command.InputMethodAction;
  module Buffer = Buffer.Impl(Editor);

  let printLog = true;
  let log =
    if (printLog) {
      Js.log;
    } else {
      _ => ();
    };

  module Instance = {
    type t = {
      mutable range: (int, int),
      mutable decoration: array(Editor.Decoration.t),
      mutable buffer: Buffer.t,
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
        buffer: Buffer.make(),
      };
    };

    let withIn = (instance, offset) => {
      let (start, end_) = instance.range;
      start <= offset && offset <= end_;
    };

    let redocorate = (instance, editor) => {
      instance.decoration->Array.forEach(Editor.Decoration.destroy);
      let (start, end_) = instance.range;
      let start = Editor.pointAtOffset(editor, start);
      let end_ = Editor.pointAtOffset(editor, end_);
      instance.decoration =
        Editor.Decoration.underlineText(
          editor,
          Editor.Range.make(start, end_),
        );
    };

    let destroy = instance => {
      log("KILLED");
      instance.decoration->Array.forEach(Editor.Decoration.destroy);
    };
  };

  type t = {
    onAction: Event.t(Command.InputMethodAction.t),
    mutable instances: array(Instance.t),
    mutable activated: bool,
    mutable cursorsToBeChecked: option(array(Editor.Point.t)),
    mutable busy: bool,
    mutable handles: array(Editor.Disposable.t),
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

    // kill the Instances that are not are not pointed by cursors
    let checkCursorPositions = (points: array(Editor.Point.t)) => {
      let offsets = points->Array.map(Editor.offsetAtPoint(editor));
      log(
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

      // store the surviving instances
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

      // emit "Deactivate" if all instances have been destroyed
      //
      if (Array.length(self.instances) == 0) {
        self.onAction.emit(Deactivate);
      };
    };

    // iterate through a list of rewrites and apply them to the text editor
    let applyRewrites = rewrites => {
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

              log(
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

              instance.range = (accum + start, accum + end_ + delta);
              // pass this change down
              let accum = accum + delta;

              // update the text buffer
              Editor.deleteText(editor, range)
              ->Promise.flatMap(_ => {
                  Editor.insertText(
                    editor,
                    Editor.Range.start(range),
                    rewriteWith,
                  )
                })
              ->Promise.flatMap(_ => {
                  Instance.redocorate(instance, editor);
                  go(accum, rewrites);
                });
            };

      go(0, List.fromArray(rewrites));
    };

    // update offsets of Instances base on changeEvents
    let updateInstanceOffsets = (changes: array(Editor.changeEvent)) => {
      // sort the changes base on their offsets in the ascending order
      let changes =
        Js.Array.sortInPlaceWith(
          (x: Editor.changeEvent, y: Editor.changeEvent) =>
            compare(x.offset, y.offset),
          changes,
        );

      // iterate through Instances and changeEvents
      // returns a list of Instances along with the changeEvent event that occurred inside that Instance
      let rec go:
        (int, (list(Editor.changeEvent), list(Instance.t))) =>
        list((Instance.t, option(Editor.changeEvent))) =
        accum =>
          fun
          | ([change, ...cs], [instance, ...is]) => {
              let (start, end_) = instance.range;
              let delta =
                String.length(change.insertText) - change.replaceLength;
              if (Instance.withIn(instance, change.offset)) {
                // `change` appears inside the `instance`
                instance.range = (accum + start, accum + end_ + delta);
                [
                  (
                    instance,
                    Some({
                      offset: change.offset + accum,
                      insertText: change.insertText,
                      replaceLength: change.replaceLength,
                    }),
                  ),
                  ...go(accum + delta, (cs, is)),
                ];
              } else if (change.offset < fst(instance.range)) {
                // `change` appears before the `instance`
                go(
                  accum + delta, // update only `accum`
                  (cs, [instance, ...is]),
                );
              } else {
                // `change` appears after the `instance`
                instance.range = (accum + start, accum + end_);
                [(instance, None), ...go(accum, ([change, ...cs], is))];
              };
            }
          | ([], [instance, ...is]) =>
            [instance, ...is]->List.map(i => (i, None))
          | (_, []) => [];
      let instancesWithChanges =
        go(0, (List.fromArray(changes), List.fromArray(self.instances)))
        ->List.toArray;

      let rewrites = [||];

      // store the updated instances
      // and push rewrites to the `rewrites` queue
      self.instances =
        instancesWithChanges->Array.keepMap(((instance, change)) => {
          switch (change) {
          | None => Some(instance)
          | Some(change) =>
            let next =
              Buffer.update(fst(instance.range), instance.buffer, change);
            switch (next) {
            | Noop => Some(instance)
            | Update(buffer) =>
              instance.buffer = buffer;
              self.onAction.emit(Update(Buffer.toSequence(buffer)));
              Some(instance);
            | Rewrite(buffer, text) =>
              Js.Array.push(
                {
                  start: fst(instance.range),
                  end_: snd(instance.range),
                  rewriteWith: text,
                  instance,
                },
                rewrites,
              )
              ->ignore;
              instance.buffer = buffer;
              self.onAction.emit(Update(Buffer.toSequence(buffer)));
              Some(instance);
            | Stuck =>
              Js.log("STUCK");
              Instance.destroy(instance);
              None;
            };
          }
        });
      rewrites;
    };

    // initiate listeners
    Editor.onChangeCursorPosition(points =>
      if (self.busy) {
        // cannot check cursor positions at this moment
        // store cursor positions and wait until the system is not busy
        self.cursorsToBeChecked =
          Some(points);
      } else {
        checkCursorPositions(points);
      }
    )
    ->Js.Array.push(self.handles)
    ->ignore;
    Editor.onChange(changes =>
      if (!self.busy && Array.length(changes) > 0) {
        // if any changes occured to the editor and the system is not busy
        self.busy = true;
        // update the offsets to reflect the changes
        let rewrites = updateInstanceOffsets(changes);
        // apply rewrites onto the text editor
        applyRewrites(rewrites)
        ->Promise.get(() => {
            // all offsets updated and rewrites applied, reset the semaphore
            self.busy = false;
            // see if there are any pending cursor positions to be checked
            switch (self.cursorsToBeChecked) {
            | None => ()
            | Some(points) => checkCursorPositions(points)
            };
          });
      }
    )
    ->Js.Array.push(self.handles)
    ->ignore;
  };

  let deactivate = self => {
    self.instances->Array.forEach(Instance.destroy);
    self.instances = [||];
    self.activated = false;
    self.cursorsToBeChecked = None;
    self.busy = false;
    self.handles->Array.forEach(Editor.Disposable.dispose);
    self.handles = [||];
  };

  let make = () => {
    onAction: Event.make(),
    instances: [||],
    activated: false,
    cursorsToBeChecked: None,
    busy: false,
    handles: [||],
  };
};