open! BsMocha.Mocha;
open! Belt;

open Test__Util;
// module Goal = Goal.Impl(Editor);
// module Task = Task.Impl(Editor);
module QueryIM = QueryIM.Impl(Editor);
// module Dispatcher = Dispatcher.Impl(Editor);
// module GoalHandler = Handle__Goal.Impl(Editor);

// module Console = Js.Console;
// module Exn = Js.Exn;
// module JsPromise = Js.Promise;
// open Promise;

module Assert = BsMocha.Assert;

let testQueryIMUpdate = (self, ~input, ~output, ~command=?, ()) => {
  let result = self->QueryIM.update(input);
  switch (result) {
  | None => Assert.fail("shouldn't stop after \"" ++ input ++ "\"")
  | Some((output', command')) =>
    Assert.equal(output', output);
    switch (command) {
    | None => ()
    | Some(command) => Assert.equal(command', command)
    };
  };
};

describe_only("Input Method (Query)", () => {
  describe_only("Insertion", () => {
    it({j|should translate "\bn" to "𝕟"|j}, () => {
      let queryIM = QueryIM.make();

      queryIM->QueryIM.activate("");

      queryIM->testQueryIMUpdate(~input={j|b|j}, ~output={j|♭|j}, ());
      queryIM->testQueryIMUpdate(
        ~input={j|♭n|j},
        ~output={j|𝕟|j},
        ~command=Deactivate,
        (),
      );
    });

    it({j|should translate "garbage \\bn" to "garbage 𝕟"|j}, () => {
      let queryIM = QueryIM.make();

      queryIM->QueryIM.activate("garbage ");

      queryIM->testQueryIMUpdate(
        ~input={j|garbage b|j},
        ~output={j|garbage ♭|j},
        (),
      );

      queryIM->testQueryIMUpdate(
        ~input={j|garbage ♭n|j},
        ~output={j|garbage 𝕟|j},
        ~command=Deactivate,
        (),
      );
    });
  });

  describe_only("Backspace", () => {
    it({j|should work just fine|j}, () => {
      let queryIM = QueryIM.make();

      queryIM->QueryIM.activate("");

      queryIM->testQueryIMUpdate(~input={j|l|j}, ~output={j|←|j}, ());
      queryIM->testQueryIMUpdate(~input={j|←a|j}, ~output={j|←a|j}, ());
      queryIM->testQueryIMUpdate(~input={j|←am|j}, ~output={j|←am|j}, ());
      queryIM->testQueryIMUpdate(
        ~input={j|←amb|j},
        ~output={j|←amb|j},
        (),
      );
      queryIM->testQueryIMUpdate(
        ~input={j|←ambd|j},
        ~output={j|←ambd|j},
        (),
      );
      queryIM->testQueryIMUpdate(~input={j|←ambda|j}, ~output={j|λ|j}, ());
      queryIM->testQueryIMUpdate(~input={j||j}, ~output={j|lambd|j}, ());
      queryIM->testQueryIMUpdate(~input={j|lambda|j}, ~output={j|λ|j}, ());
      queryIM->testQueryIMUpdate(~input={j|λb|j}, ~output={j|λb|j}, ());
      queryIM->testQueryIMUpdate(~input={j|λba|j}, ~output={j|λba|j}, ());
      queryIM->testQueryIMUpdate(
        ~input={j|λbar|j},
        ~output={j|ƛ|j},
        ~command=Deactivate,
        (),
      );
    })
  });
});
