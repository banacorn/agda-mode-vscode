open! BsMocha.Mocha
open Test__Util

@module("vscode") @scope("commands")
external executeCommand: string => Promise.t<option<State.t>> = "executeCommand"

describe("Tokens", ~timeout=10000, () => {
  Q.it("should receive tokens from Agda", () => {
    let filepath = Path.asset("GotoDefinition.agda")
    Temp.activateExtensionAndOpenFile(filepath)
    ->Promise.flatMap(_ => executeCommand("agda-mode.load"))
    ->Promise.flatMap(state => {
      switch state {
      | None => Promise.resolved(Error(Exn("Cannot load " ++ filepath)))
      | Some(state) =>
        let tokens =
          state.tokens
          ->Tokens.toArray
          ->Belt.Array.map(((token, range)) =>
            Editor.Range.toString(range) ++ " " ++ Tokens.Token.toString(token)
          )

        A.deep_equal(
          tokens,
          [
            "0:0-6 Token (0, 6) [1]",
            "0:7-21 Token (7, 21) [28] [src: 1]",
            "0:22-27 Token (22, 27) [1]",
            "1:0-4 Token (28, 32) [1]",
            "1:5-6 Token (33, 34) [25] [src: 34]",
            "1:7-8 Token (35, 36) [4]",
            "1:9-12 Token (37, 40) [30] [src: 326]",
            "1:13-18 Token (41, 46) [1]",
            "2:2-3 Token (49, 50) [23] [src: 50]",
            "2:4-5 Token (51, 52) [4]",
            "2:6-7 Token (53, 54) [25] [src: 34]",
            "3:2-3 Token (57, 58) [23] [src: 58]",
            "3:4-5 Token (59, 60) [4]",
            "3:6-7 Token (61, 62) [25] [src: 34]",
            "3:8-9 Token (63, 64) [4]",
            "3:10-11 Token (65, 66) [25] [src: 34]",
            "5:0-3 Token (68, 71) [27, 34] [src: 69]",
            "5:4-5 Token (72, 73) [4]",
            "5:6-7 Token (74, 75) [25] [src: 34]",
            "5:8-9 Token (76, 77) [4]",
            "5:10-11 Token (78, 79) [25] [src: 34]",
            "5:12-13 Token (80, 81) [4]",
            "5:14-15 Token (82, 83) [25] [src: 34]",
            "6:0-1 Token (84, 85) [21] [src: 85]",
            "6:2-3 Token (86, 87) [27, 34] [src: 69]",
            "6:4-5 Token (88, 89) [21] [src: 89]",
            "6:6-7 Token (90, 91) [4]",
            "6:8-15 Token (92, 99) [34]",
          ],
        )
      }
    })
  })
})
