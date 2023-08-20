type t = {
  // the symbol at the front of the sequence along with the sequence it replaced
  symbol: option<(string, string)>,
  // the sequence following the symbol we see on the text editor
  tail: string,
  // current translation of the underlying sequence
  translation: Translator.translation,
  // index of the candidateSymbol of translation
  candidateIndex: int,
}

// examples of Buffer.t:
//    user typed: l       => { symbol: Some("←", "l"), tail: "" }
//    user typed: lambd   => { symbol: Some("←", "l"), tail: "ambd" }
//    user typed: lambda   => { symbol: Some("λ", "lambda"), tail: "" }

let make = () => {
  symbol: None,
  tail: "",
  translation: Translator.translate("", None),
  candidateIndex: 0,
}

let isEmpty = self => self.symbol == None && self.tail == ""

let toSequence = self =>
  switch self.symbol {
  | None => self.tail
  | Some((_, sequence)) => sequence ++ self.tail
  }

let toSurface = self =>
  switch self.symbol {
  | None => self.tail
  | Some((symbol, _)) => symbol ++ self.tail
  }

let toString = self => "\"" ++ (toSurface(self) ++ ("\"[" ++ (toSequence(self) ++ "]")))

let moveUp = self => {
  ...self,
  candidateIndex: max(0, self.candidateIndex - 10),
}

let moveRight = self => {
  ...self,
  candidateIndex: min(Array.length(self.translation.candidateSymbols) - 1, self.candidateIndex + 1),
}

let moveDown = self => {
  ...self,
  candidateIndex: min(
    Array.length(self.translation.candidateSymbols) - 1,
    self.candidateIndex + 10,
  ),
}

let moveLeft = self => {
  ...self,
  candidateIndex: max(0, self.candidateIndex - 1),
}

type change = {
  offset: int,
  insertedText: string,
  replacedTextLength: int,
}

let update = (self, start, change: change): (t, option<string>) => {
  let sequence = toSequence(self)
  // some modification has been made to the sequence
  // devise the new sequence
  let newSequence = {
    // calculate where the replacement started
    let insertStartInTextEditor = change.offset - start
    // since the actual replaced text may contain a symbol
    // we need `+ String.length(symbolSequence) - String.length(symbol)`
    // to get the actual offset in the sequence
    let insertStart = switch self.symbol {
    | Some((symbol, symbolSequence)) =>
      insertStartInTextEditor + String.length(symbolSequence) - String.length(symbol)
    | None => insertStartInTextEditor
    }
    let insertEnd = insertStart + change.replacedTextLength

    let beforeInsertedText = Js.String.substring(~from=0, ~to_=insertStart, sequence)
    let afterInsertedText = Js.String.substringToEnd(~from=insertEnd, sequence)
    beforeInsertedText ++ (change.insertedText ++ afterInsertedText)
  }

  let translation = Translator.translate(newSequence, Some({lastTranslation: self.translation, candidateIndex: self.candidateIndex}))
  switch translation.symbol {
  | None =>
    if translation.further {
      if Js.String.includes(sequence, newSequence) {
        // special case of INSERTION
        // reduce unnecessary rewriting
        let diff = Js.String.substringToEnd(~from=String.length(sequence), newSequence)
        let buffer = {...self, tail: self.tail ++ diff, translation: translation}
        (buffer, None)
      } else {
        let buffer = {
          symbol: None,
          tail: newSequence,
          translation: translation,
          candidateIndex: self.candidateIndex,
        }
        (buffer, Some(toSurface(buffer)))
      }
    } else {
      let buffer = {...self, translation: translation}
      (buffer, None)
    }
  | Some(symbol) =>
    let buffer = {
      symbol: Some((symbol, newSequence)),
      tail: "",
      translation: translation,
      candidateIndex: self.candidateIndex,
    }
    (buffer, Some(toSurface(buffer)))
  }
}
