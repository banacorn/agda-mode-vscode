// open Belt;

module Impl = (Editor: Sig.Editor) => {
  type t = {
    // the symbol at the front of the sequence along with the sequence it replaced
    symbol: option((string, string)),
    // the sequence following the symbol we see on the text editor
    tail: string,
  };

  // examples of Buffer.t:
  //    user typed: l       => { symbol: Some("←", "l"), tail: "" }
  //    user typed: lambd   => { symbol: Some("←", "l"), tail: "ambd" }
  //    user typed: lambda   => { symbol: Some("λ", "lambda"), tail: "" }

  let init = string =>
    Js.String.substring(~from=0, ~to_=String.length(string) - 1, string);

  let make = () => {symbol: None, tail: ""};

  let isEmpty = self => {
    self.symbol == None && self.tail == "";
  };

  let toSequence = self =>
    switch (self.symbol) {
    | None => self.tail
    | Some((_, sequence)) => sequence ++ self.tail
    };

  let toSurface = self =>
    switch (self.symbol) {
    | None => self.tail
    | Some((symbol, _)) => symbol ++ self.tail
    };

  let toString = self =>
    "\"" ++ toSurface(self) ++ "\"[" ++ toSequence(self) ++ "]" /*   }*/;

  // type action =
  //   | Noop
  //   | Insert(t) // update the buffer accordingly
  //   | Backspace(t) // update the buffer accordingly
  //   | Rewrite(t) // should rewrite the text buffer
  //   | Complete // should deactivate
  //   | Stuck(int); // should deactivate, too

  type action =
    | Noop
    | Stuck
    | Update(t)
    | Rewrite(t, string);

  let init = string =>
    Js.String.substring(~from=0, ~to_=String.length(string) - 1, string);

  let update = (start, self, change: Editor.changeEvent) => {
    let sequence = toSequence(self);
    // Js.log(
    //   "REPLACE "
    //   ++ string_of_int(change.offset)
    //   ++ " "
    //   ++ change.insertText
    //   ++ " "
    //   ++ string_of_int(change.replaceLength)
    //   ++ " "
    //   ++ toSurface(self),
    // );
    if (toSurface(self) == change.insertText) {
      Noop;
    } else if (change.insertText == " ") {
      Stuck;
    } else {
      // some modification has been made to the sequence
      // devise the new sequence
      let newSequence = {
        // calculate where the replacement started
        let insertStartInTextEditor = change.offset - start;
        // since the actual replaced text may contain a symbol
        // we need `+ String.length(symbolSequence) - String.length(symbol)`
        // to get the actual offset in the sequence
        let insertStart =
          switch (self.symbol) {
          | Some((symbol, symbolSequence)) =>
            insertStartInTextEditor
            + String.length(symbolSequence)
            - String.length(symbol)
          | None => insertStartInTextEditor
          };
        let insertEnd = insertStart + change.replaceLength;

        let beforeInsertedText =
          Js.String.substring(~from=0, ~to_=insertStart, sequence);
        let afterInsertedText =
          Js.String.substringToEnd(~from=insertEnd, sequence);
        // Js.log(
        //   beforeInsertedText
        //   ++ "["
        //   ++ change.insertText
        //   ++ "]"
        //   ++ afterInsertedText,
        // );
        beforeInsertedText ++ change.insertText ++ afterInsertedText;
      };

      if (Js.String.includes(sequence, newSequence)) {
        // INSERTION
        let diff =
          Js.String.substringToEnd(
            ~from=String.length(sequence),
            newSequence,
          );
        let translation = Translator.translate(newSequence);
        switch (translation.symbol) {
        | None =>
          // Special case of INSERTION
          let buffer = {symbol: self.symbol, tail: self.tail ++ diff};
          Update(buffer);
        | Some(symbol) =>
          let buffer = {symbol: Some((symbol, newSequence)), tail: ""};
          Rewrite(buffer, toSurface(buffer));
        };
      } else {
        let translation = Translator.translate(newSequence);
        switch (translation.symbol) {
        | None =>
          let buffer = {symbol: None, tail: newSequence};
          Rewrite(buffer, toSurface(buffer));
        | Some(symbol) =>
          let buffer = {symbol: Some((symbol, newSequence)), tail: ""};
          Rewrite(buffer, toSurface(buffer));
        };
      };
    };
  };
};