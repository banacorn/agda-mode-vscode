open! BsMocha.Mocha;
open! Belt;

module Assert = BsMocha.Assert;

let testPromptIMUpdate = (self, ~input, ~output, ~command=?, ()) => {
  let result = self->PromptIM.update(input);
  switch (result) {
  | None => Assert.fail("shouldn't be deactivated after \"" ++ input ++ "\"")
  | Some((output', command')) =>
    Assert.equal(output', output);
    switch (command) {
    | None => ()
    | Some(command) => Assert.equal(command', command)
    };
  };
};

describe("Input Method (Prompt)", () => {
  describe("Insertion", () => {
    it({j|should translate "\bn" to "𝕟"|j}, () => {
      let promptIM = PromptIM.make();

      promptIM->PromptIM.activate("");

      promptIM->testPromptIMUpdate(~input={j|b|j}, ~output={j|♭|j}, ());
      promptIM->testPromptIMUpdate(
        ~input={j|♭n|j},
        ~output={j|𝕟|j},
        ~command=Deactivate,
        (),
      );
    });

    it({j|should translate "garbage \\bn" to "garbage 𝕟"|j}, () => {
      let promptIM = PromptIM.make();

      promptIM->PromptIM.activate("garbage ");

      promptIM->testPromptIMUpdate(
        ~input={j|garbage b|j},
        ~output={j|garbage ♭|j},
        (),
      );

      promptIM->testPromptIMUpdate(
        ~input={j|garbage ♭n|j},
        ~output={j|garbage 𝕟|j},
        ~command=Deactivate,
        (),
      );
    });
  });

  describe("Backspacing", () => {
    it({j|should work just fine|j}, () => {
      let promptIM = PromptIM.make();

      promptIM->PromptIM.activate("");

      promptIM->testPromptIMUpdate(~input={j|l|j}, ~output={j|←|j}, ());
      promptIM->testPromptIMUpdate(~input={j|←a|j}, ~output={j|←a|j}, ());
      promptIM->testPromptIMUpdate(
        ~input={j|←am|j},
        ~output={j|←am|j},
        (),
      );
      promptIM->testPromptIMUpdate(
        ~input={j|←amb|j},
        ~output={j|←amb|j},
        (),
      );
      promptIM->testPromptIMUpdate(
        ~input={j|←ambd|j},
        ~output={j|←ambd|j},
        (),
      );
      promptIM->testPromptIMUpdate(
        ~input={j|←ambda|j},
        ~output={j|λ|j},
        (),
      );
      promptIM->testPromptIMUpdate(~input={j||j}, ~output={j|lambd|j}, ());
      promptIM->testPromptIMUpdate(~input={j|lamb|j}, ~output={j|lamb|j}, ());
      promptIM->testPromptIMUpdate(
        ~input={j|lambd|j},
        ~output={j|lambd|j},
        (),
      );
      promptIM->testPromptIMUpdate(~input={j|lambda|j}, ~output={j|λ|j}, ());
      promptIM->testPromptIMUpdate(~input={j|λb|j}, ~output={j|λb|j}, ());
      promptIM->testPromptIMUpdate(~input={j|λba|j}, ~output={j|λba|j}, ());
      promptIM->testPromptIMUpdate(
        ~input={j|λbar|j},
        ~output={j|ƛ|j},
        ~command=Deactivate,
        (),
      );
    })
  });
});
