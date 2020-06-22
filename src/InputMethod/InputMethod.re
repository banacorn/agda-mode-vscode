open Belt;

module Impl = (Editor: Sig.Editor) => {
  // open Command.InputMethodAction;
  module Buffer = Buffer.Impl(Editor);

  let printLog = false;
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
      instance.decoration = [||];
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
      instance.decoration->Array.forEach(Editor.Decoration.destroy);
      instance.decoration = [||];
    };
  };

  type t = {
    onAction: Event.t(Command.InputMethod.t),
    mutable instances: array(Instance.t),
    mutable activated: bool,
    mutable cursorsToBeChecked: option(array(Editor.Point.t)),
    mutable busy: bool,
    mutable handles: array(Editor.Disposable.t),
  };

  // datatype for representing a rewrite to be made to the text editor
  type rewrite = {
    range: (int, int),
    text: string,
    instance: option(Instance.t) // update range offsets after rewrite
  };

  let insertBackslash = editor => {
    Editor.getCursorPositions(editor)
    ->Array.forEach(point => {
        Editor.insertText(editor, point, "\\")->ignore
      });
  };
  let insertChar = (editor, char) => {
    let char = Js.String.charAt(0, char);
    Editor.getCursorPositions(editor)
    ->Array.forEach(point => {
        Editor.insertText(editor, point, char)->ignore
      });
  };

  // kill the Instances that are not are not pointed by cursors
  let checkCursorPositions = (self, editor, points: array(Editor.Point.t)) => {
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
          let survived = offsets->Belt.Array.some(Instance.withIn(instance));
          // if not, the instance gets destroyed
          if (!survived) {
            Instance.destroy(instance);
          };
          survived;
        });

    self.cursorsToBeChecked = None;

    // emit "Deactivate" if all instances have been destroyed
    if (Array.length(self.instances) == 0) {
      self.onAction.emit(Deactivate);
    };
  };

  let updateView = self => {
    // update the view
    self.instances[0]
    ->Option.forEach(instance => {
        self.onAction.emit(
          Update(
            Buffer.toSequence(instance.buffer),
            instance.buffer.translation,
            instance.buffer.candidateIndex,
          ),
        )
      });
  };

  // iterate through a list of rewrites and apply them to the text editor
  let applyRewrites = (self, editor, rewrites) => {
    let rec go: (int, list(rewrite)) => Promise.t(unit) =
      accum =>
        fun
        | [] => Promise.resolved()
        | [{range, text, instance}, ...rewrites] => {
            let (start, end_) = range;
            // this value represents the offset change made by this update
            // e.g. "lambda" => "λ" would result in `-5`
            let delta = String.length(text) - (end_ - start);

            log(
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

            let editorRange =
              Editor.Range.make(
                Editor.pointAtOffset(editor, start + accum),
                Editor.pointAtOffset(editor, end_ + accum),
              );
            // update the text buffer
            Editor.deleteText(editor, editorRange)
            ->Promise.flatMap(_ => {
                Editor.insertText(
                  editor,
                  Editor.Range.start(editorRange),
                  text,
                )
              })
            ->Promise.flatMap(_ => {
                // update range offsets and redecorate the Instance
                // if it still exist after this rewrite
                switch (instance) {
                | None => ()
                | Some(instance) =>
                  instance.range = (accum + start, accum + end_ + delta);
                  Instance.redocorate(instance, editor);
                };
                // pass this change down
                go(accum + delta, rewrites);
              });
          };

    // before apply edits to the text editor, flip the semaphore
    self.busy = true;
    // iterate though the list of rewrites
    go(0, List.fromArray(rewrites))
    ->Promise.get(() => {
        // all offsets updated and rewrites applied, reset the semaphore
        self.busy = false;
        // see if there are any pending cursor positions to be checked
        switch (self.cursorsToBeChecked) {
        | None => ()
        | Some(points) => checkCursorPositions(self, editor, points)
        };
      });
  };

  let moveUp = (self, editor) => {
    let rewrites =
      self.instances
      ->Array.keepMap(instance => {
          instance.buffer = Buffer.moveUp(instance.buffer);
          instance.buffer.translation.candidateSymbols[instance.buffer.
                                                         candidateIndex]
          ->Option.map(symbol => {
              {range: instance.range, text: symbol, instance: Some(instance)}
            });
        });
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);

    // update the view
    updateView(self);
  };

  let moveRight = (self, editor) => {
    let rewrites =
      self.instances
      ->Array.keepMap(instance => {
          instance.buffer = Buffer.moveRight(instance.buffer);
          instance.buffer.translation.candidateSymbols[instance.buffer.
                                                         candidateIndex]
          ->Option.map(symbol => {
              {range: instance.range, text: symbol, instance: Some(instance)}
            });
        });
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);
    // update the view
    updateView(self);
  };

  let moveDown = (self, editor) => {
    let rewrites =
      self.instances
      ->Array.keepMap(instance => {
          instance.buffer = Buffer.moveDown(instance.buffer);
          instance.buffer.translation.candidateSymbols[instance.buffer.
                                                         candidateIndex]
          ->Option.map(symbol => {
              {range: instance.range, text: symbol, instance: Some(instance)}
            });
        });
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);
    // update the view
    updateView(self);
  };

  let moveLeft = (self, editor) => {
    let rewrites =
      self.instances
      ->Array.keepMap(instance => {
          instance.buffer = Buffer.moveLeft(instance.buffer);
          instance.buffer.translation.candidateSymbols[instance.buffer.
                                                         candidateIndex]
          ->Option.map(symbol => {
              {range: instance.range, text: symbol, instance: Some(instance)}
            });
        });
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);
    // update the view
    updateView(self);
  };

  let chooseSymbol = (self, editor, symbol) => {
    let rewrites =
      self.instances
      ->Array.map(instance => {
          {range: instance.range, text: symbol, instance: Some(instance)}
        });
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);
  };

  let activate = (self, editor, offsets: array(int)) => {
    // instantiate from an array of offsets
    self.instances =
      Js.Array.sortInPlaceWith(compare, offsets)
      ->Array.map(Instance.make(editor));

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
            let (buffer, shouldRewrite) =
              Buffer.reflectEditorChange(
                instance.buffer,
                fst(instance.range),
                change,
              );
            // issue rewrites
            shouldRewrite->Option.forEach(text => {
              Js.Array.push(
                {
                  range: instance.range,
                  text,
                  instance:
                    buffer.translation.further ? Some(instance) : None,
                },
                rewrites,
              )
              ->ignore
            });

            // destroy the instance if there's no further possible transition
            if (buffer.translation.further) {
              instance.buffer = buffer;
              Some(instance);
            } else {
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
        checkCursorPositions(self, editor, points);
      }
    )
    ->Js.Array.push(self.handles)
    ->ignore;
    Editor.onChange(changes =>
      if (!self.busy && Array.length(changes) > 0) {
        // update the offsets to reflect the changes
        let rewrites = updateInstanceOffsets(changes);
        // apply rewrites onto the text editor
        applyRewrites(self, editor, rewrites);
        // update the view
        updateView(self);
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