open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  //   // the places where the input method is activated
  //   let startingOffsets: array(int) =
  //     Editor.getCursorPositions(state.editor)
  //     ->Array.map(Editor.offsetAtPoint(state.editor));
  //   Js.log(
  //     "start listening "
  //     ++ startingOffsets->Array.map(string_of_int)->Util.Pretty.array,
  //   );
  //   // let cursor = ref(startingOffset);
  //   let handle = ref(None);
  //   let listener = change => {
  //     change->Js.log;
  //   };
  //   handle :=
  //     Some(Editor.onChange(changes => changes->Array.forEach(listener)));

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
      Js.log("KILL");
      instance.decoration->Array.forEach(Editor.Decoration.destroy);
    };
  };

  //   type t = array(Instance.t);

  let activate = (editor, offsets: array(int)) => {
    // instantiate from an array of offsets
    let instances: ref(array(Instance.t)) =
      ref(offsets->Array.map(Instance.make(editor)));

    let find = (instances, offset) =>
      instances->Array.keep((instance: Instance.t) => {
        let (_start, end_) = instance.range;
        // start <= offset && offset <= end_;
        offset == end_;
      });

    // handles of listeners
    let editorChangeHandle = ref(None);
    let cursorChangeHandle = ref(None);

    // destroy the handles if all Instances are destroyed
    let checkIfEveryonesAlive = () =>
      if (Array.length(instances^) == 0) {
        Js.log("ALL DEAD");
        (editorChangeHandle^)->Option.forEach(Editor.Disposable.dispose);
        (cursorChangeHandle^)->Option.forEach(Editor.Disposable.dispose);
      };

    // listeners
    let editorChangelistener = (change: Editor.changeEvent) => {
      instances :=
        (instances^)
        ->Array.map((instance: Instance.t) => {
            let (start, end_) = instance.range;
            if (Instance.withIn(instance, change.offset)) {
              Js.log(change);
              // update the range
              instance.range = (
                start,
                end_ + String.length(change.insertText) - change.replaceLength,
              );
            };
            instance;
          });
    };

    // kill the Instances that are not are not pointed by cursors
    let cursorChangelistener = (points: array(Editor.Point.t)) => {
      checkIfEveryonesAlive();
      let offsets = points->Array.map(Editor.offsetAtPoint(editor));
      offsets->Array.map(string_of_int)->Util.Pretty.array->Js.log;
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
    };
    // initiate listeners
    editorChangeHandle :=
      Some(
        Editor.onChange(changes => {
          checkIfEveryonesAlive();
          changes->Array.forEach(editorChangelistener);
        }),
      );
    cursorChangeHandle :=
      Some(Editor.onChangeCursorPosition(cursorChangelistener));
  };
};