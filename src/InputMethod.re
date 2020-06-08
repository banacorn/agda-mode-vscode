open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  open Command.InputMethodAction;

  module Instance = {
    type t = {
      mutable range: (int, int),
      decoration: array(Editor.Decoration.t),
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

  let activate =
      (
        editor,
        offsets: array(int),
        onAction: Event.t(Command.InputMethodAction.t),
      ) => {
    // instantiate from an array of offsets
    let instances: ref(array(Instance.t)) =
      ref(offsets->Js.Array.sortInPlace->Array.map(Instance.make(editor)));

    // handles of listeners
    let editorChangeHandle = ref(None);
    let cursorChangeHandle = ref(None);

    // destroy the handles if all Instances are destroyed
    let checkIfEveryoneIsStillAlive = () =>
      if (Array.length(instances^) == 0) {
        Js.log("ALL DEAD");
        onAction.emit(Deactivate);
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
      let rec scan:
        (int, (list(Editor.changeEvent), list(Instance.t))) =>
        list(Instance.t) =
        accum =>
          fun
          | ([change, ...cs], [instance, ...is]) => {
              let (start, end_) = instance.range;
              let delta =
                String.length(change.insertText) - change.replaceLength;

              if (Instance.withIn(instance, change.offset)) {
                // `change` appears inside the `instance`
                instance.range = (accum + start, accum + end_ + delta);
                [instance, ...scan(accum + delta, (cs, is))];
              } else if (change.offset < fst(instance.range)) {
                // `change` appears before the `instance`
                scan(
                  accum + delta, // update only `accum`
                  (cs, [instance, ...is]),
                );
              } else {
                // `change` appears after the `instance`
                instance.range = (accum + start, accum + end_);
                [instance, ...scan(accum, ([change, ...cs], is))];
              };
            }
          | ([], [instance, ...is]) => [instance, ...is]
          | (_, []) => [];
      instances :=
        scan(0, (List.fromArray(changes), List.fromArray(instances^)))
        ->List.toArray;
    };

    // kill the Instances that are not are not pointed by cursors
    let cursorChangelistener = (points: array(Editor.Point.t)) => {
      let offsets = points->Array.map(Editor.offsetAtPoint(editor));
      // Js.log("CURSORS");
      // offsets->Array.map(string_of_int)->Util.Pretty.array->Js.log;
      instances :=
        (instances^)
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
};