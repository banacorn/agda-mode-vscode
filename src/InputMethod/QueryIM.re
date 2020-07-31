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

  // given a string from the Query input
  // update the Buffer and return the rewritten string and the next Command (if any)
  let update = (self, input) => {
    let inputLength = String.length(input);

    let before = Js.String.substring(~from=0, ~to_=self.start, input);
    let isInsertion = {
      let expected = before ++ Buffer.toSurface(self.buffer);
      let actual = Js.String.substring(~from=0, ~to_=inputLength - 1, input);
      actual == expected;
    };
    let isBackspace = {
      let expected = before;
      let actual = input;
      actual == expected;
    };

    // INSERT
    if (isInsertion) {
      let insertedText =
        Js.String.substringToEnd(~from=inputLength - 1, input);
      let change: Editor.changeEvent = {
        offset: inputLength - 1,
        insertText: insertedText,
        replaceLength: 0,
      };
      let (buffer, shouldRewrite) =
        Buffer.reflectEditorChange(self.buffer, self.start, change);
      self.buffer = buffer;

      if (buffer.translation.further) {
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
        | Some(_) => Some((before ++ Buffer.toSurface(buffer), Deactivate))
        | None => Some((input, Deactivate))
        };
      };
    } else if (isBackspace) {
      let change: Editor.changeEvent = {
        offset: inputLength,
        insertText: "",
        replaceLength: 1,
      };
      let (buffer, shouldRewrite) =
        Buffer.reflectEditorChange(self.buffer, self.start, change);
      self.buffer = buffer;

      if (buffer.translation.further) {
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
        | Some(_) => Some((before ++ Buffer.toSurface(buffer), Deactivate))
        | None => Some((before ++ Buffer.toSurface(buffer), Deactivate))
        };
      };
    } else {
      self.buffer = Buffer.make();
      self.activated = false;
      None;
    };
  };
};
