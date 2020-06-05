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

  module Cursor = {
    type t = int;
  };

  type t = {mutable traced: array(int)};

  let activate = (editor, offsets: array(int)) => {
    let cursors: t = {traced: offsets};

    let find = (cursors, offset) => cursors.traced
                                     ->Array.keep(x => x == offset)[0];

    let handle = ref(None);
    let listener = (change: Editor.changeEvent) => {
      Js.log(change);
      let changedCursor = find(cursors, change.offset);
      switch (changedCursor) {
      | None => ()
      | Some(cursor) => Js.log(string_of_int(cursor) ++ " changed")
      };
      ();
    };
    handle :=
      Some(Editor.onChange(changes => changes->Array.forEach(listener)));
    ();
  };
};