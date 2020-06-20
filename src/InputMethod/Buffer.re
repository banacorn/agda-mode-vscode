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
    "\"" ++ toSurface(self) ++ "\"[" ++ toSequence(self) ++ "]";

  type state = {
    buffer: t,
    translation: Translator.translation,
    shouldRewrite: option(string),
  };

  let init = string =>
    Js.String.substring(~from=0, ~to_=String.length(string) - 1, string);

  let reflectEditorChange = (self, start, change: Editor.changeEvent) => {
    let sequence = toSequence(self);
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

    let translation = Translator.translate(newSequence);
    switch (translation.symbol) {
    | None =>
      if (translation.further) {
        if (Js.String.includes(sequence, newSequence)) {
          // special case of INSERTION
          // reduce unnecessary rewriting
          let diff =
            Js.String.substringToEnd(
              ~from=String.length(sequence),
              newSequence,
            );
          let buffer = {symbol: self.symbol, tail: self.tail ++ diff};
          {buffer, translation, shouldRewrite: None};
        } else {
          let buffer = {symbol: None, tail: newSequence};
          {buffer, translation, shouldRewrite: Some(toSurface(buffer))};
        };
      } else {
        {buffer: self, translation, shouldRewrite: None};
      }
    | Some(symbol) =>
      let buffer = {symbol: Some((symbol, newSequence)), tail: ""};
      {buffer, translation, shouldRewrite: Some(toSurface(buffer))};
    };
  };
};