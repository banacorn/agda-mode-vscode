// open Belt;

module Impl = (Editor: Sig.Editor) => {
  // open Command.InputMethodAction;
  module Buffer = Buffer.Impl(Editor);

  type t = {
    mutable buffer: Buffer.t,
    start: int,
    mutable end_: int,
  };

  let make = n => {buffer: Buffer.make(), start: n, end_: n};

  let update = (self, input) => {
    let penult = String.length(input) - 1;
    let inputInit = Js.String.substring(~from=0, ~to_=penult, input);
    let inputLast = Js.String.substringToEnd(~from=penult, input);

    // INSERT
    if (inputInit == Buffer.toSurface(self.buffer)) {
      let change: Editor.changeEvent = {
        offset: penult,
        insertText: inputLast,
        replaceLength: 0,
      };
      let (buffer, shouldRewrite) =
        Buffer.reflectEditorChange(self.buffer, self.start, change);
      self.buffer = buffer;

      switch (shouldRewrite) {
      | None =>
        Js.log(
          "input: "
          ++ input
          ++ "\ninputInit: "
          ++ inputInit
          ++ "\ninputLast: "
          ++ inputLast
          ++ "\nbuffer: "
          ++ Buffer.toString(self.buffer),
        )
      | Some(_) => ()
      };

      Some(Buffer.toSurface(buffer));
    } else {
      Js.log(
        "input: " ++ input ++ "\nbuffer" ++ Buffer.toString(self.buffer),
      );
      None;
    };
  };
};
