// open! BsMocha.Mocha
// open! Test__Util
// open Belt

// describe("Tokens", ~timeout=10000, () => {
//   let context = Agda.make("GotoDefinition.agda")

//   describe("GotoDefinition.agda", () => {
//     Q.it("should produce 28 tokens", () => {
//       context
//       ->Promise.flatMap(Agda.load)
//       ->Promise.flatMapOk(((_, state)) => {
//         let tokens =
//           state.tokens
//           ->Tokens.toArray
//           ->Belt.Array.map(((token, range)) =>
//             Editor.Range.toString(range) ++ " " ++ Tokens.Token.toString(token)
//           )
//         A.deepEqual(28, Array.length(tokens))
//       })
//     })

//     Q.it("should produce correct tokens", () => {
//       context
//       ->Promise.flatMap(Agda.load)
//       ->Promise.flatMapOk(((_, state)) => {
//         let tokens =
//           state.tokens
//           ->Tokens.toArray
//           ->Belt.Array.map(((token, range)) =>
//             Editor.Range.toString(range) ++ " " ++ Tokens.Token.toString(token)
//           )
//         A.deepEqual(
//           [
//             "0:0-6 Token (0, 6) [Keyword]",
//             "0:7-21 Token (7, 21) [Module] [src: 1]",
//             "0:22-27 Token (22, 27) [Keyword]",
//             "1:0-4 Token (28, 32) [Keyword]",
//             "1:5-6 Token (33, 34) [Datatype] [src: 34]",
//             "1:7-8 Token (35, 36) [Symbol]",
//             "1:9-12 Token (37, 40) [Primitive] [src: 388]",
//             "1:13-18 Token (41, 46) [Keyword]",
//             "2:2-3 Token (49, 50) [ConstructorInductive] [src: 50]",
//             "2:4-5 Token (51, 52) [Symbol]",
//             "2:6-7 Token (53, 54) [Datatype] [src: 34]",
//             "3:2-3 Token (57, 58) [ConstructorInductive] [src: 58]",
//             "3:4-5 Token (59, 60) [Symbol]",
//             "3:6-7 Token (61, 62) [Datatype] [src: 34]",
//             "3:8-9 Token (63, 64) [Symbol]",
//             "3:10-11 Token (65, 66) [Datatype] [src: 34]",
//             "5:0-3 Token (68, 71) [Function, Operator] [src: 69]",
//             "5:4-5 Token (72, 73) [Symbol]",
//             "5:6-7 Token (74, 75) [Datatype] [src: 34]",
//             "5:8-9 Token (76, 77) [Symbol]",
//             "5:10-11 Token (78, 79) [Datatype] [src: 34]",
//             "5:12-13 Token (80, 81) [Symbol]",
//             "5:14-15 Token (82, 83) [Datatype] [src: 34]",
//             "6:0-1 Token (84, 85) [Bound] [src: 85]",
//             "6:2-3 Token (86, 87) [Function, Operator] [src: 69]",
//             "6:4-5 Token (88, 89) [Bound] [src: 89]",
//             "6:6-7 Token (90, 91) [Symbol]",
//             "6:8-15 Token (92, 99) [Hole]",
//           ],
//           tokens,
//         )
//       })
//     })
//   })
// })
