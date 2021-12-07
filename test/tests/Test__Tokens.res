open! BsMocha.Mocha
open Test__Util
open LanguageServerMule.Source
open Belt

type setup = ref<option<Connection.Emacs.t>>

// module for checking if Agda is present in PATH
module Agda = {
  module Error = {
    type t =
      | LanguageServerMuleErrors(array<Error.t>)
      | EmacsConnectionError(Connection.Emacs.Error.t)
    let toString = x =>
      switch x {
      | LanguageServerMuleErrors(errors) =>
        Js.Array.joinWith(",", errors->Array.map(LanguageServerMule.Source.Error.toString))
      | EmacsConnectionError(error) =>
        let (header, body) = Connection.Emacs.Error.toString(error)
        "EmacsConnectionError: " ++ header ++ ": " ++ body
      }
  }

  let exists = command =>
    Module.searchUntilSuccess([FromCommand(command)])
    ->Promise.flatMap(((result, errors)) =>
      switch result {
      | None => Promise.resolved(Error(errors))
      | Some(_method) => Promise.resolved(Ok())
      }
    )
    ->Promise.flatMapError(errors => {
      let msg = Js.Array.joinWith(",", errors->Array.map(LanguageServerMule.Source.Error.toString))
      A.fail("Cannot find \"Agda\" in PATH: " ++ msg)
    })
}

let acquire = setup =>
  switch setup.contents {
  | None => A.fail("Cannot acquire the setup")
  | Some(setup) => Promise.resolved(Ok(setup))
  }

let cleanup = (setup: setup) => {
  switch setup.contents {
  | None => Promise.resolved()
  | Some(conn) => Connection.Emacs.destroy(conn)
  }
}

describe("Tokens", ~timeout=10000, () => {
  let tokens = ref(None)

  let acquire = () =>
    switch tokens.contents {
    | None => Promise.resolved(Error(Exn("Cannot acquire tokens")))
    | Some(tokens) => Promise.resolved(Ok(tokens))
    }

  Q.before(() => {
    let filepath = Path.asset("GotoDefinition.agda")

    Config.inTestingMode := true
    Config.Connection.setAgdaVersion("agda")
    ->Promise.flatMap(() => Config.Connection.setUseAgdaLanguageServer(false))
    ->Promise.flatMap(() => Agda.exists("agda"))
    ->Promise.flatMap(_ => activateExtensionAndOpenFile(filepath)->Promise.map(x => Ok(x)))
    ->Promise.flatMapOk(_ => executeCommand("agda-mode.load")->Promise.map(state => Ok(state)))
    ->Promise.flatMapOk(state => {
      switch state {
      | None => Promise.resolved(Error(Exn("Cannot load " ++ filepath)))
      | Some(Ok(state)) =>
        tokens :=
          Some(
            state.tokens
            ->Tokens.toArray
            ->Belt.Array.map(((token, range)) =>
              Editor.Range.toString(range) ++ " " ++ Tokens.Token.toString(token)
            ),
          )
        Promise.resolved(Ok())
      | Some(Error(error)) =>
        let (header, body) = Connection.Error.toString(error)
        Promise.resolved(Error(Exn(header ++ "\n" ++ body)))
      }
    })
  })

  describe("GotoDefinition.agda", () => {
    Q.it("should produce 28 tokens", () => {
      acquire()->Promise.flatMapOk(tokens => {
        A.deep_equal(28, Array.length(tokens))
      })
    })

    Q.it("should produce correct tokens", () => {
      acquire()->Promise.flatMapOk(tokens => {
        A.deep_equal(
          [
            "0:0-6 Token (0, 6) [Keyword]",
            "0:7-21 Token (7, 21) [Module] [src: 1]",
            "0:22-27 Token (22, 27) [Keyword]",
            "1:0-4 Token (28, 32) [Keyword]",
            "1:5-6 Token (33, 34) [Datatype] [src: 34]",
            "1:7-8 Token (35, 36) [Symbol]",
            "1:9-12 Token (37, 40) [Primitive] [src: 326]",
            "1:13-18 Token (41, 46) [Keyword]",
            "2:2-3 Token (49, 50) [ConstructorInductive] [src: 50]",
            "2:4-5 Token (51, 52) [Symbol]",
            "2:6-7 Token (53, 54) [Datatype] [src: 34]",
            "3:2-3 Token (57, 58) [ConstructorInductive] [src: 58]",
            "3:4-5 Token (59, 60) [Symbol]",
            "3:6-7 Token (61, 62) [Datatype] [src: 34]",
            "3:8-9 Token (63, 64) [Symbol]",
            "3:10-11 Token (65, 66) [Datatype] [src: 34]",
            "5:0-3 Token (68, 71) [Function, Operator] [src: 69]",
            "5:4-5 Token (72, 73) [Symbol]",
            "5:6-7 Token (74, 75) [Datatype] [src: 34]",
            "5:8-9 Token (76, 77) [Symbol]",
            "5:10-11 Token (78, 79) [Datatype] [src: 34]",
            "5:12-13 Token (80, 81) [Symbol]",
            "5:14-15 Token (82, 83) [Datatype] [src: 34]",
            "6:0-1 Token (84, 85) [Bound] [src: 85]",
            "6:2-3 Token (86, 87) [Function, Operator] [src: 69]",
            "6:4-5 Token (88, 89) [Bound] [src: 89]",
            "6:6-7 Token (90, 91) [Symbol]",
            "6:8-15 Token (92, 99) [Hole]",
          ],
          tokens,
        )
      })
    })
  })
})
