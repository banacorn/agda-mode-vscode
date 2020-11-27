open! BsMocha.Mocha
open! Belt

module Assert = BsMocha.Assert

let testPromptIMUpdate = (self, ~input, ~output, ~command=?, ()) => {
  let result = self->PromptIM.update(input)
  switch result {
  | None => Assert.fail("shouldn't be deactivated after \"" ++ (input ++ "\""))
  | Some((output', command')) =>
    Assert.equal(output', output)
    switch command {
    | None => ()
    | Some(command) => Assert.equal(command', command)
    }
  }
}

describe("Input Method (Prompt)", () => {
  describe("Insertion", () => {
    it(j`should translate "\\bn" to "𝕟"`, () => {
      let promptIM = PromptIM.make()

      promptIM->PromptIM.activate("")

      promptIM->testPromptIMUpdate(~input=j`b`, ~output=j`♭`, ())
      promptIM->testPromptIMUpdate(~input=j`♭n`, ~output=j`𝕟`, ~command=Deactivate, ())
    })

    it(j`should translate "garbage \\\\bn" to "garbage 𝕟"`, () => {
      let promptIM = PromptIM.make()

      promptIM->PromptIM.activate("garbage ")

      promptIM->testPromptIMUpdate(~input=j`garbage b`, ~output=j`garbage ♭`, ())

      promptIM->testPromptIMUpdate(
        ~input=j`garbage ♭n`,
        ~output=j`garbage 𝕟`,
        ~command=Deactivate,
        (),
      )
    })
  })

  describe("Backspacing", () => it(j`should work just fine`, () => {
      let promptIM = PromptIM.make()

      promptIM->PromptIM.activate("")

      promptIM->testPromptIMUpdate(~input=j`l`, ~output=j`←`, ())
      promptIM->testPromptIMUpdate(~input=j`←a`, ~output=j`←a`, ())
      promptIM->testPromptIMUpdate(~input=j`←am`, ~output=j`←am`, ())
      promptIM->testPromptIMUpdate(~input=j`←amb`, ~output=j`←amb`, ())
      promptIM->testPromptIMUpdate(~input=j`←ambd`, ~output=j`←ambd`, ())
      promptIM->testPromptIMUpdate(~input=j`←ambda`, ~output=j`λ`, ())
      promptIM->testPromptIMUpdate(~input=j``, ~output=j`lambd`, ())
      promptIM->testPromptIMUpdate(~input=j`lamb`, ~output=j`lamb`, ())
      promptIM->testPromptIMUpdate(~input=j`lambd`, ~output=j`lambd`, ())
      promptIM->testPromptIMUpdate(~input=j`lambda`, ~output=j`λ`, ())
      promptIM->testPromptIMUpdate(~input=j`λb`, ~output=j`λb`, ())
      promptIM->testPromptIMUpdate(~input=j`λba`, ~output=j`λba`, ())
      promptIM->testPromptIMUpdate(~input=j`λbar`, ~output=j`ƛ`, ~command=Deactivate, ())
    }))
})
