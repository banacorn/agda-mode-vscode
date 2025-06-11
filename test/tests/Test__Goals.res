open Mocha
open Test__Util

// Assults on Goals, for property-based testing
module Assult = {
  type t = Move(Tokens.Change.t) // only moves boundaries without damaging them

  let toString = assult =>
    switch assult {
    | Move(change) => "Move(" ++ Tokens.Change.toString(change) ++ ")"
    }

  // let toChange = assult =>
  //   switch assult {
  //   | Move(offset, delta) => {
  //         offset: offset,
  //         removed: int, // length of the removed text
  //         inserted: int, // length of the inserted text
  //       }
  //   }
  //     // Tokens.Change.make(
  //     //   VSCode.Range.make(VSCode.Position.make(0, offset), VSCode.Position.make(0, offset + 1)),
  //     //   String.repeat(" ", delta),
  //     // )
  //   }

  open FastCheck.Arbitrary

  let arbitraryMoveAfter = (goals: array<Goal2.t>, after) => {
    // return the gaps between the boundaries of the goals after the given offset
    let gapsBetweenBoundariesAfter =
      goals
      ->Array.reduce(([], 0), ((acc, prevEnd), goal) => (
        if prevEnd >= after {
          if goal.end - goal.start >= 4 {
            // between !}  {! and between {! !}
            [(prevEnd, goal.start), (goal.start + 2, goal.end - 2), ...acc]
          } else {
            // between !}  {!
            [(prevEnd, goal.start), ...acc]
          }
        } else {
          acc
        },
        goal.end,
      ))
      ->fst

    let length = Array.length(gapsBetweenBoundariesAfter)

    let pickedGap = if length == 0 {
      Combinators.constant(None)
    } else {
      integerRange(0, length - 1)->Derive.map(index => gapsBetweenBoundariesAfter[index])
    }

    pickedGap->Derive.chain(gap =>
      switch gap {
      | None =>
        //  no gaps, generate a arbitrary move
        integerRange(after, after + 20)->Derive.chain(offset => {
          integerRange(0, 10)->Derive.chain(
            inserted =>
              integerRange(0, 10)->Derive.map(removed => Move({offset, inserted, removed})),
          )
        })
      | Some((gapStart, gapEnd)) =>
        integerRange(gapStart, gapEnd)->Derive.chain(offset => {
          // at most `offset - gapStart` characters can be removed
          // no limit on how many characters can be inserted
          integerRange(0, offset - gapStart)->Derive.chain(
            removed => {
              integerRange(0, 10)->Derive.map(inserted => Move({offset, inserted, removed}))
            },
          )
        })
      }
    )
  }

  let arbitraryMoveWithGoals = () =>
    Goal2.arbitraryBatch()->Derive.chain(goals =>
      arbitraryMoveAfter(goals, 0)->Derive.map(move => (goals, move))
    )
}

describe("Goals", () => {
  let fileContent = ref("")

  Async.before(async () => fileContent := (await File.read(Path.asset("Goals.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))
  Async.after(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))

  describe("On document change", () => {
    Async.it(
      "should instantiate all 4 goals with question marks expanded to holes",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        await ctx->AgdaMode.quit
        // compare file content before and after
        let actual = await File.read(Path.asset("Goals.agda"))
        let expected = await File.read(Path.asset("Goals.agda.out"))
        Assert.deepStrictEqual(actual, expected)
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
      },
    )

    // open FastCheck
    // open Property.Sync

    // describe(
    //   "Move only changes",
    //   () => {
    //     it_only(
    //       "should result in only move actions",
    //       () =>
    //         assert_(
    //           property1(
    //             Assult.arbitraryMoveWithGoals(),
    //             ((goals, assult)) => {
    //               Js.log("\nassult:    " ++ assult->Assult.toString)
    //               Js.log("goals:      " ++ goals->Array.map(Goal2.toString)->Util.Pretty.array)

    //               let readText = (_, _) => ""
    //               let goals = goals->Array.map(goal => (goal.start, goal.end))

    //               let actions = switch assult {
    //               | Move(change) => Goals.actionsFromChanges(readText, goals, [change])
    //               }

    //               actions->Array.every(
    //                 action =>
    //                   switch action {
    //                   | Goals.UpdatePosition(_, _) => true
    //                   | _ => false
    //                   },
    //               )
    //             },
    //           ),
    //         ),
    //     )
    //   },
    // )
  })

  Async.it("should destroy a goal after it has been completely deleted", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    let _ = await Editor.Text.delete(
      ctx.state.document,
      VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26)),
    )
    await ctx->AgdaMode.quit
    // check the positions of the goals
    Assert.deepStrictEqual(
      Goals.serialize(ctx.state.goals2),
      ["#0 [92-99)", "#1 [118-125)", "#3 [164-168)"],
    )
  })

  Async.it("should destroy a goal after it has been completely replaced", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    let _ = await Editor.Text.replace(
      ctx.state.document,
      VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26)),
      "       ",
    )
    await ctx->AgdaMode.quit
    // check the positions of the goals
    Assert.deepStrictEqual(
      Goals.serialize(ctx.state.goals2),
      ["#0 [92-99)", "#1 [118-125)", "#3 [164-168)"],
    )
  })

  Async.it("should only resize a goal after its content has been edited", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    let _ = await Editor.Text.replace(
      ctx.state.document,
      VSCode.Range.make(VSCode.Position.make(9, 22), VSCode.Position.make(9, 23)),
      ":D",
    )
    await ctx->AgdaMode.quit

    // check the positions of the goals
    Assert.deepStrictEqual(
      Goals.serialize(ctx.state.goals2),
      ["#0 [92-99)", "#1 [118-125)", "#2 [145-153)", "#3 [172-176)"],
    )
  })

  describe("Restore hole damanged boundaries", () => {
    Async.it(
      "should protect against a backspace on the right boundary",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.delete(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(9, 25), VSCode.Position.make(9, 26)),
        )
        await ctx->AgdaMode.quit

        // check the file content
        let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
        let actual = Editor.Text.get(ctx.state.document, range)
        Assert.deepStrictEqual(actual, "{!   !}")
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
      },
    )

    Async.it(
      "should protect against a deletion on the right boundary",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.delete(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(9, 24), VSCode.Position.make(9, 25)),
        )
        await ctx->AgdaMode.quit

        // check the file content
        let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
        let actual = Editor.Text.get(ctx.state.document, range)
        Assert.deepStrictEqual(actual, "{!   !}")
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
      },
    )

    Async.it(
      "should protect against a backspace on the left boundary",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.delete(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(9, 20), VSCode.Position.make(9, 21)),
        )
        await ctx->AgdaMode.quit

        // check the file content
        let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
        let actual = Editor.Text.get(ctx.state.document, range)
        Assert.deepStrictEqual(actual, "{!   !}")
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
      },
    )

    Async.it(
      "should protect against a deletion on the left boundary",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.delete(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 20)),
        )
        await ctx->AgdaMode.quit

        // check the file content
        let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
        let actual = Editor.Text.get(ctx.state.document, range)
        Assert.deepStrictEqual(actual, "{!   !}")
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
      },
    )
  })

  describe("Jumping between goals", () => {
    Async.it(
      "should jump to the next goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(0, 0))

        // should land on the first goal
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(7, 14))

        // should land on the second goal
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(8, 21))

        // should land on the third goal
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(9, 22))

        // should land on the fourth goal
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 20))

        // should land on the first goal again
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(7, 14))
      },
    )

    Async.it(
      "should jump to the previous goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(0, 0))

        // should land on the last goal
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 20))

        // should land on the third goal
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(9, 22))

        // should land on the second goal
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(8, 21))

        // should land on the first goal
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(7, 14))

        // should land on the last goal again
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 20))
      },
    )
  })
})
