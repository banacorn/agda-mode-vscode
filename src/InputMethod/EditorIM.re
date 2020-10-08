open Belt;

module Impl = (Editor: Sig.Editor) => {
  type event =
    | Change
    | Activate
    | Deactivate;

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

    let make = (editor, range) => {
      let document = Editor.getDocument(editor);
      let start = Editor.pointAtOffset(document, fst(range));
      let end_ = Editor.pointAtOffset(document, snd(range));
      {
        range,
        decoration: [|
          Editor.Decoration.underlineText(
            editor,
            Editor.Range.make(start, end_),
          ),
        |],
        buffer: Buffer.make(),
      };
    };

    let withIn = (instance, offset) => {
      let (start, end_) = instance.range;
      start <= offset && offset <= end_;
    };

    let redocorate = (instance, editor) => {
      let document = Editor.getDocument(editor);
      instance.decoration->Array.forEach(Editor.Decoration.destroy);
      instance.decoration = [||];
      let (start, end_) = instance.range;
      let start = Editor.pointAtOffset(document, start);
      let end_ = Editor.pointAtOffset(document, end_);
      instance.decoration = [|
        Editor.Decoration.underlineText(
          editor,
          Editor.Range.make(start, end_),
        ),
      |];
    };

    let destroy = instance => {
      instance.decoration->Array.forEach(Editor.Decoration.destroy);
      instance.decoration = [||];
    };
  };

  type t = {
    mutable instances: array(Instance.t),
    mutable activated: bool,
    mutable cursorsToBeChecked: option(array(Editor.Point.t)),
    mutable busy: bool,
    mutable handles: array(Editor.Disposable.t),
    // for notifying the Task Dispatcher
    eventEmitter: Event.t(Command.InputMethod.t),
    // for reporting when some task has be done
    eventEmitterTest: Event.t(event),
  };

  let fromEditorChangeEvent = (change: Editor.changeEvent): Buffer.change => {
    offset: change.offset,
    insertedText: change.insertedText,
    replacedTextLength: change.replacedTextLength,
  };

  // datatype for representing a rewrite to be made to the text editor
  type rewrite = {
    rangeBefore: (int, int),
    rangeAfter: (int, int),
    text: string,
    instance: option(Instance.t) // update range offsets after rewrite
  };

  // kill the Instances that are not are not pointed by cursors
  let checkCursorPositions = (self, document, points: array(Editor.Point.t)) => {
    let offsets = points->Array.map(Editor.offsetAtPoint(document));
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
      self.eventEmitter.emit(Deactivate);
      self.eventEmitterTest.emit(Deactivate);
    };
  };

  let updateView = self => {
    // update the view
    self.instances[0]
    ->Option.forEach(instance => {
        self.eventEmitter.emit(
          Update(
            Buffer.toSequence(instance.buffer),
            instance.buffer.translation,
            instance.buffer.candidateIndex,
          ),
        )
      });
  };

  let toRewrites =
      (instances: array(Instance.t), f: Instance.t => option(string))
      : array(rewrite) => {
    let accum = ref(0);

    instances->Array.keepMap(instance => {
      let (start, end_) = instance.range;
      // update the range
      instance.range = (start + accum^, end_ + accum^);

      f(instance)
      ->Option.map(replacement => {
          let delta = String.length(replacement) - (end_ - start);
          // update `accum`
          accum := accum^ + delta;
          // returns a `rewrite`
          {
            rangeBefore: instance.range,
            rangeAfter: (fst(instance.range), snd(instance.range) + delta),
            text: replacement,
            instance: Some(instance),
          };
        });
    });
  };

  // iterate through a list of rewrites and apply them to the text editor
  let applyRewrites = (self, editor, rewrites) => {
    let document = Editor.getDocument(editor);
    // before applying edits to the text editor, flip the semaphore
    self.busy = true;
    // iterate though the list of rewrites
    rewrites
    ->Array.map(({rangeBefore, rangeAfter, text, instance}, ()) => {
        let editorRange =
          Editor.Range.make(
            Editor.pointAtOffset(document, fst(rangeBefore)),
            Editor.pointAtOffset(document, snd(rangeBefore)),
          );
        // update the text buffer
        Editor.deleteText(document, editorRange)
        ->Promise.flatMap(_ => {
            Editor.insertText(
              document,
              Editor.Range.start(editorRange),
              text,
            )
          })
        ->Promise.map(_ => {
            // update range offsets and redecorate the Instance
            // if it still exist after this rewrite
            switch (instance) {
            | None => ()
            | Some(instance) =>
              instance.range = rangeAfter;
              Instance.redocorate(instance, editor);
            }
          });
      })
    ->Util.oneByOne
    ->Promise.get(_ => {
        // emit CHANGE event after applied rewriting
        self.eventEmitterTest.emit(Change);
        // all offsets updated and rewrites applied, reset the semaphore
        self.busy = false;
        // see if there are any pending cursor positions to be checked
        switch (self.cursorsToBeChecked) {
        | None => ()
        | Some(points) => checkCursorPositions(self, document, points)
        };
      });
  };

  // update offsets of Instances base on changes
  let updateInstanceOffsets =
      (instances: array(Instance.t), changes: array(Buffer.change))
      : (array(Instance.t), array(rewrite)) => {
    // sort the changes base on their offsets in the ascending order
    let changes =
      Js.Array.sortInPlaceWith(
        (x: Buffer.change, y: Buffer.change) => compare(x.offset, y.offset),
        changes,
      );

    // iterate through Instances and changes
    // returns a list of Instances along with the changeEvent event that occurred inside that Instance
    let rec go:
      (int, (list(Buffer.change), list(Instance.t))) =>
      list((Instance.t, option(Buffer.change))) =
      accum =>
        fun
        | ([change, ...cs], [instance, ...is]) => {
            let (start, end_) = instance.range;
            let delta =
              String.length(change.insertedText) - change.replacedTextLength;
            if (Instance.withIn(instance, change.offset)) {
              // `change` appears inside the `instance`
              instance.range = (accum + start, accum + end_ + delta);
              [
                (instance, Some({...change, offset: change.offset + accum})),
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
      go(0, (List.fromArray(changes), List.fromArray(instances)))
      ->List.toArray;

    let rewrites = [||];

    // push rewrites to the `rewrites` queue
    let instances = {
      let accum = ref(0);
      instancesWithChanges->Array.keepMap(((instance, change)) => {
        switch (change) {
        | None => Some(instance)
        | Some(change) =>
          let (buffer, shouldRewrite) =
            Buffer.update(instance.buffer, fst(instance.range), change);
          // issue rewrites
          shouldRewrite->Option.forEach(text => {
            let (start, end_) = instance.range;
            let delta = String.length(text) - (end_ - start);
            Js.Array.push(
              {
                rangeBefore: (start + accum^, end_ + accum^),
                rangeAfter: (start + accum^, end_ + accum^ + delta),
                text,
                instance: buffer.translation.further ? Some(instance) : None,
              },
              rewrites,
            )
            ->ignore;
            accum := accum^ + delta;
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
    };

    (instances, rewrites);
  };

  let activate = (self, editor, ranges: array((int, int))) => {
    self.activated = true;

    // emit ACTIVATE event after applied rewriting
    self.eventEmitterTest.emit(Activate);
    // setContext
    Editor.setContext("agdaModeTyping", true)->ignore;

    // instantiate from an array of offsets
    self.instances =
      Js.Array.sortInPlaceWith((x, y) => compare(fst(x), fst(y)), ranges)
      ->Array.map(Instance.make(editor));

    // initiate listeners
    Editor.onChangeCursorPosition(points =>
      if (self.busy) {
        // cannot check cursor positions at this moment
        // store cursor positions and wait until the system is not busy
        self.cursorsToBeChecked =
          Some(points);
      } else {
        checkCursorPositions(self, Editor.getDocument(editor), points);
      }
    )
    ->Js.Array.push(self.handles)
    ->ignore;

    // listens to changes from the text editor
    Editor.onChange(changes =>
      if (!self.busy && Array.length(changes) > 0) {
        let changes = changes->Array.map(fromEditorChangeEvent);
        // update the offsets to reflect the changes
        let (instances, rewrites) =
          updateInstanceOffsets(self.instances, changes);
        self.instances = instances;
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
    // setContext
    Editor.setContext("agdaModeTyping", false)->ignore;

    self.eventEmitterTest.emit(Deactivate);

    self.instances->Array.forEach(Instance.destroy);
    self.instances = [||];
    self.activated = false;
    self.cursorsToBeChecked = None;
    self.busy = false;
    self.handles->Array.forEach(Editor.Disposable.dispose);
    self.handles = [||];
  };

  let make = eventEmitterTest => {
    instances: [||],
    activated: false,
    cursorsToBeChecked: None,
    busy: false,
    handles: [||],
    eventEmitter: Event.make(),
    eventEmitterTest,
  };
  ////////////////////////////////////////////////////////////////////////////////////////////

  let moveUp = (self, editor) => {
    let rewrites =
      toRewrites(
        self.instances,
        instance => {
          instance.buffer = Buffer.moveUp(instance.buffer);
          instance.buffer.translation.candidateSymbols[instance.buffer.
                                                         candidateIndex];
        },
      );
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);

    // update the view
    updateView(self);
  };

  let moveRight = (self, editor) => {
    let rewrites =
      toRewrites(
        self.instances,
        instance => {
          instance.buffer = Buffer.moveRight(instance.buffer);
          instance.buffer.translation.candidateSymbols[instance.buffer.
                                                         candidateIndex];
        },
      );
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);
    // update the view
    updateView(self);
  };

  let moveDown = (self, editor) => {
    let rewrites =
      toRewrites(
        self.instances,
        instance => {
          instance.buffer = Buffer.moveDown(instance.buffer);
          instance.buffer.translation.candidateSymbols[instance.buffer.
                                                         candidateIndex];
        },
      );

    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);
    // update the view
    updateView(self);
  };

  let moveLeft = (self, editor) => {
    let rewrites =
      toRewrites(
        self.instances,
        instance => {
          instance.buffer = Buffer.moveLeft(instance.buffer);
          instance.buffer.translation.candidateSymbols[instance.buffer.
                                                         candidateIndex];
        },
      );

    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);
    // update the view
    updateView(self);
  };

  let chooseSymbol = (self, editor, symbol) => {
    let rewrites = toRewrites(self.instances, _ => {Some(symbol)});
    // apply rewrites onto the text editor
    applyRewrites(self, editor, rewrites);
    // update the view
    updateView(self);
  };

  let insertBackslash = editor => {
    Editor.getCursorPositions(editor)
    ->Array.forEach(point => {
        Editor.insertText(Editor.getDocument(editor), point, "\\")->ignore
      });
  };
  let insertChar = (editor, char) => {
    let char = Js.String.charAt(0, char);
    Editor.getCursorPositions(editor)
    ->Array.forEach(point => {
        Editor.insertText(Editor.getDocument(editor), point, char)->ignore
      });
  };
};
