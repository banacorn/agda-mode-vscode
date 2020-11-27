module type Module = {
  type t
  let make: unit => t
  let activate: (t, string) => unit
  let deactivate: t => unit
  let isActivated: t => bool

  let update: (t, string) => option<(string, Command.InputMethod.t)>
  let insertChar: (t, string) => option<(string, Command.InputMethod.t)>

  let chooseSymbol: (t, string) => bool
}

module Module: Module = {
  type t = {
    mutable activated: bool,
    mutable buffer: Buffer.t,
    mutable textBeforeActivation: string,
  }

  let make = () => {
    activated: false,
    buffer: Buffer.make(),
    textBeforeActivation: "",
  }

  let activate = (self, text) => {
    self.activated = true
    self.textBeforeActivation = text
  }

  let deactivate = self => {
    self.activated = false
    self.buffer = Buffer.make()
    self.textBeforeActivation = ""
  }

  let isActivated = self => self.activated

  // helper funcion
  let init = s => Js.String.substring(~from=0, ~to_=String.length(s) - 1, s)
  let last = s => Js.String.substringToEnd(~from=String.length(s) - 1, s)

  // devise the "change" made to the input box
  let deviseChange = (self, input): option<Buffer.change> => {
    let inputLength = String.length(input)
    let bufferSurface = Buffer.toSurface(self.buffer)

    if init(input) == self.textBeforeActivation ++ bufferSurface {
      // Insertion
      Some({
        offset: inputLength - 1,
        insertedText: last(input),
        replacedTextLength: 0,
      })
    } else if (
      input == self.textBeforeActivation ||
        input == self.textBeforeActivation ++ init(bufferSurface)
    ) {
      // Backspacing
      Some({
        offset: inputLength,
        insertedText: "",
        replacedTextLength: 1,
      })
    } else {
      None
    }
  }

  // given a string from the Prompt input
  // update the Buffer and return the rewritten string and the next Command (if any)
  let update = (self, input) =>
    switch deviseChange(self, input) {
    | Some(change) =>
      let (buffer, shouldRewrite) = Buffer.update(
        self.buffer,
        String.length(self.textBeforeActivation),
        change,
      )
      if buffer.translation.further {
        self.buffer = buffer
        Some((
          self.textBeforeActivation ++ Buffer.toSurface(buffer),
          Command.InputMethod.Update(
            Buffer.toSequence(buffer),
            buffer.translation,
            buffer.candidateIndex,
          ),
        ))
      } else {
        self.buffer = Buffer.make()
        self.activated = false
        switch shouldRewrite {
        | Some(s) => Some((self.textBeforeActivation ++ s, Deactivate))
        | None => Some((input, Deactivate))
        }
      }
    | None =>
      deactivate(self)
      None
    }

  let insertChar = (self, char) =>
    update(self, self.textBeforeActivation ++ (Buffer.toSurface(self.buffer) ++ char))

  let chooseSymbol = (self, symbol) => {
    switch self.buffer.symbol {
    | None => false
    | Some((_, symbolSequence)) =>
      self.buffer = {
        ...self.buffer,
        symbol: Some((symbol, symbolSequence)),
      }
      true
    }
  }
}

include Module
