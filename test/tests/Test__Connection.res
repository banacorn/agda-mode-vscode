// open Mocha
// open Test__Util

// describe("Connection", () => {
//   describe("Picked connection", () => {
//     Async.it_only(
//       "should return the previously picked connection",
//       async () => {
//         // let ctx = await AgdaMode.makeAndLoad("Load.agda")

//         // setup the memento
//         let expected = Connection.Target.Agda("2.7.0.1", "path/to/agda")
//         let memento = State__Memento.make(None)
//         await Connection.Target.setPicked(memento, Some(expected))

//         let paths = ["path/to/agda", "path/to/als"]

//         let actual = await Connection__Target.getPicked(memento, paths)

//         Assert.equal(actual, Some(expected))

//         // await ctx->AgdaMode.refine(~cursor=VSCode.Position.make(12, 9))
//         // let actual = await File.read(Path.asset("Issue158.agda"))
//         // let expected = await File.read(Path.asset("Issue158.agda.out"))
//         // Assert.equal(actual, expected)
//       },
//     )
//   })
// })
