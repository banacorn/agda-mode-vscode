open Mocha
open Test__Util

describe_only("Goals", () => {
  let fileContent = ref("")

  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Goals.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))

  Async.it("should instantiate all 4 goals with question marks expanded to holes", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    // compare file content before and after
    let actual = await File.read(Path.asset("Goals.agda"))
    let expected = await File.read(Path.asset("Goals.agda.out"))
    Assert.deepStrictEqual(actual, expected)
    // check the positions of the goals
    Assert.deepStrictEqual(
      Goals.serialize(ctx.state.goals2),
      ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-178)"],
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
      Goals.serialize(ctx.state.goals2),
      ["#0 [92-99)", "#1 [118-125)", "#3 [164-171)"],
    )
  })

  Async.it("should only resize a goal after its content has been edited", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    let _ = await Editor.Text.replace(
      ctx.state.document,
      VSCode.Range.make(VSCode.Position.make(9, 22), VSCode.Position.make(9, 23)),
      ":D",
    )
    // check the positions of the goals
    Assert.deepStrictEqual(
      Goals.serialize(ctx.state.goals2),
      ["#0 [92-99)", "#1 [118-125)", "#2 [145-153)", "#3 [172-179)"],
    )
  })

  // Async.it("should restore a goal after it has been partially damaged", async () => {
  //   let ctx = await AgdaMode.makeAndLoad("Goals.agda")
  //   let _ = await Editor.Text.delete(
  //     ctx.state.document,
  //     VSCode.Range.make(VSCode.Position.make(9, 18), VSCode.Position.make(9, 26)),
  //   )
  //   // check the positions of the goals
  //   Assert.deepStrictEqual(
  //     ctx.state.goals2->Goals.getGoals,
  //     [(0, (92, 99)), (1, (118, 125)), (2, (145, 152)), (3, (171, 178))]->Map.fromArray,
  //   )
  // })
})
