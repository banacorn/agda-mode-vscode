// open Belt;

module Impl = (Editor: Sig.Editor) => {
  // open Command.InputMethodAction;
  module Buffer = Buffer.Impl(Editor);

  type t = {
    mutable activated: bool,
    mutable start: int,
    mutable buffer: Buffer.t,
    // // for notifying the Task Dispatcher
    // eventEmitter: Event.t(Command.InputMethod.t),
  };

  let make = () => {
    activated: false,
    buffer: Buffer.make(),
    // to mark where the input method begins (where backslash "\" is typed)
    start: 0,
  };

  let activate = (self, text) => {
    self.activated = true;
    self.start = String.length(text);
  };
  // helper funcion
  let init = s => Js.String.substring(~from=0, ~to_=String.length(s) - 1, s);
  let last = s => Js.String.substringToEnd(~from=String.length(s) - 1, s);

  // devise the "change" made to the input box
  let determineChange = (self, input): option(Editor.changeEvent) => {
    let before = Js.String.substring(~from=0, ~to_=self.start, input);
    let inputLength = String.length(input);
    let bufferSurface = Buffer.toSurface(self.buffer);

    if (init(input) == before ++ bufferSurface) {
      // Insertion
      Some({
        offset: inputLength - 1,
        insertText: last(input),
        replaceLength: 0,
      });
    } else if (input == before || input == before ++ init(bufferSurface)) {
      // Backspacing
      Some({
        offset: inputLength,
        insertText: "",
        replaceLength: 1,
      });
    } else {
      None;
    };
  };

  // given a string from the Query input
  // update the Buffer and return the rewritten string and the next Command (if any)
  let update = (self, input) => {
    let before = Js.String.substring(~from=0, ~to_=self.start, input);

    switch (determineChange(self, input)) {
    | Some(change) =>
      let (buffer, shouldRewrite) =
        Buffer.reflectEditorChange(self.buffer, self.start, change);
      if (buffer.translation.further) {
        self.buffer = buffer;
        Some((
          before ++ Buffer.toSurface(buffer),
          Command.InputMethod.Update(
            Buffer.toSequence(buffer),
            buffer.translation,
            buffer.candidateIndex,
          ),
        ));
      } else {
        self.buffer = Buffer.make();
        self.activated = false;
        switch (shouldRewrite) {
        | Some(s) => Some((before ++ s, Deactivate))
        | None => Some((input, Deactivate))
        };
      };
    | None =>
      self.buffer = Buffer.make();
      self.activated = false;
      None;
    };
  };
};
