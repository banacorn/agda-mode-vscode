open Mocha
open Test__Util

describe_only("Goals", () => {
  let fileContent = ref("")

  Async.before(async () => fileContent := (await File.read(Path.asset("Goals.agda"))))
  Async.after(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))

  Async.it("should instantiate all 4 goals with question marks expanded to holes", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    // compare file content before and after
    let actual = await File.read(Path.asset("Goals.agda"))
    let expected = await File.read(Path.asset("Goals.agda.out"))
    Assert.deepStrictEqual(actual, expected)
    // check the positions of the goals
    Assert.deepStrictEqual(
      ctx.state.goals2->Goals.getGoals,
      [(0, (92, 99)), (1, (118, 125)), (2, (145, 152)), (3, (171, 178))]->Map.fromArray,
    )
  })

  Async.it("should remove a goal after it has been completely destroyed", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    let _ = await Editor.Text.delete(
      ctx.state.document,
      VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26)),
    )
    // check the positions of the goals
    Assert.deepStrictEqual(
      ctx.state.goals2->Goals.getGoals,
      [(0, (92, 99)), (1, (118, 125)), (3, (164, 171))]->Map.fromArray,
    )

    // Assert.deepStrictEqual(actual, expected)
    // check the positions of the goals
    // let positions = ctx.state.goals2->Goals.getPositions
    // Assert.deepStrictEqual(positions, [(92, 99), (118, 125), (145, 152), (171, 178)])
  })
})
