open Mocha
open Test__Util

describe("issue #264 minimal reproducer", () => {
  This.timeout(10000)
  let caseSplitFileContent = ref("")
  Async.beforeEach(async () => {
    caseSplitFileContent := await File.read(Path.asset("CaseSplit.agda"))
  })
  Async.afterEach(async () => {
    await File.write(Path.asset("CaseSplit.agda"), caseSplitFileContent.contents)
    await Registry.removeAndDestroyAll()
  })

  let serializeOffsetRanges = (document, ranges) =>
    ranges->Array.map(((start, end)) =>
      Editor.Range.toString(
        VSCode.Range.make(
          VSCode.TextDocument.positionAt(document, start),
          VSCode.TextDocument.positionAt(document, end),
        ),
      )
    )

  let extractSerializedGoalRange = goal => {
    let left = String.indexOf(goal, "[")
    let right = String.lastIndexOf(goal, ")")
    if left == -1 || right == -1 || right <= left + 1 {
      goal
    } else {
      String.slice(goal, ~start=left + 1, ~end=right)
    }
  }

  let parseActualHolePositions = raw => {
    let rec findBracketHoles = (offset, acc) => {
      let remaining = String.sliceToEnd(raw, ~start=offset)
      switch String.indexOf(remaining, "{!") {
      | -1 => acc
      | localStart =>
        let start = offset + localStart
        let afterStart = String.sliceToEnd(raw, ~start=start + 2)
        switch String.indexOf(afterStart, "!}") {
        | -1 => acc
        | localEnd =>
          let end_ = start + 2 + localEnd + 2
          findBracketHoles(end_, [...acc, (start, end_)])
        }
      }
    }

    let delimiters = [" ", "\t", "\n", "\r", "(", ")", "{", "}", "_", ";", ".", "\\", "\"", "@"]
    let isDelimiter = char => delimiters->Array.includes(char)
    let rec findQuestionMarks = (text, offset, acc) => {
      switch String.indexOf(text, "?") {
      | -1 => acc
      | index =>
        let actualPosition = offset + index
        let beforeChar = if actualPosition > 0 {
          Some(String.charAt(raw, actualPosition - 1))
        } else {
          None
        }
        let afterChar = if actualPosition < String.length(raw) - 1 {
          Some(String.charAt(raw, actualPosition + 1))
        } else {
          None
        }
        let beforeOk = switch beforeChar {
        | None => true
        | Some(char) => isDelimiter(char)
        }
        let afterOk = switch afterChar {
        | None => true
        | Some(char) => isDelimiter(char)
        }
        if beforeOk && afterOk {
          findQuestionMarks(String.sliceToEnd(text, ~start=index + 1), actualPosition + 1, [
            (actualPosition, actualPosition + 1),
            ...acc,
          ])
        } else {
          findQuestionMarks(String.sliceToEnd(text, ~start=index + 1), actualPosition + 1, acc)
        }
      }
    }

    let questionMarks = findQuestionMarks(raw, 0, [])
    let bracketHoles = findBracketHoles(0, [])
    [questionMarks, bracketHoles]->Array.flat->Array.toSorted(((a1, _), (a2, _)) => Int.compare(a1, a2))
  }

  Async.it(
    "issue #264: concurrent edit during case-split should not cause infinite goal multiplication",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("CaseSplit.agda")

      let (loadDispatched, resolveLoadDispatched, _) = Util.Promise_.pending()
      let (interactionPointsHeld, resolveInteractionPointsHeld, _) = Util.Promise_.pending()
      let (release, resolveRelease, _) = Util.Promise_.pending()
      let sawLoadDispatched = ref(false)
      let sawInteractionPointsHeld = ref(false)
      let released = ref(false)

      let releaseOnce = () =>
        if !released.contents {
          released := true
          resolveRelease()
        }

      // Log listener to detect Load command dispatch
      let disposeLog = ctx.state.channels.log->Chan.on(log => {
        switch log {
        | CommandDispatched(Load) if !sawLoadDispatched.contents =>
          sawLoadDispatched := true
          resolveLoadDispatched()
        | _ => ()
        }
      })

      // Middleware to hold InteractionPoints response, then release on command
      ctx.state.middlewares->Array.push(handler => async response => {
        switch response {
        | Response.InteractionPoints(_) if sawLoadDispatched.contents && !sawInteractionPointsHeld.contents =>
          sawInteractionPointsHeld := true
          resolveInteractionPointsHeld()
          await release
          await handler(response)
        | _ => await handler(response)
        }
      })

      let caseSplit = AgdaMode.case(ctx, ~cursor=VSCode.Position.make(8, 11), ~payload="x")

      try {
        // Wait for Load to be dispatched and InteractionPoints to be held
        await loadDispatched
        await interactionPointsHeld

        // Concurrent user edit: insert at (0, 0) without awaiting
        let userInsert = Editor.Text.insert(ctx.state.document, VSCode.Position.make(0, 0), "z")

        // Release the held InteractionPoints response
        releaseOnce()

        // Wait for both case-split and user insert to complete
        let _ = await Promise.all([
          caseSplit->Promise.thenResolve(_ => ()),
          userInsert->Promise.thenResolve(_ => ()),
        ])

        // Wait for goals to settle (should timeout or complete without proliferation)
        await ctx.state.goals->Goals.waitUntilNotBusy

        let _ = disposeLog
      } catch {
      | exn =>
        releaseOnce()
        let _ = disposeLog
        raise(exn)
      }

      // Check final state
      let goals = Goals.serializeGoals(ctx.state.goals)
      let goalRanges = goals->Array.map(extractSerializedGoalRange)
      let actualHolePositions = Editor.Text.getAll(ctx.state.document)->parseActualHolePositions
      let actualHoleRanges = serializeOffsetRanges(ctx.state.document, actualHolePositions)
      let finalDocText = Editor.Text.getAll(ctx.state.document)
      let finalDocLines = Js.String.split("\n", finalDocText)

      // Check for bug signatures: doubled markers or excessive goal counts
      let multiplicationMatches =
        finalDocLines
        ->Array.filter(line =>
          line->String.includes("{!   !}!") || line->String.includes("{!   !}{!")
        )
        ->Array.length

      let duplicateRanges =
        goalRanges->Array.filter(r => goalRanges->Array.filter(x => x == r)->Array.length > 1)

      // Before fix: expect RED (multiplication or goal loss)
      // After fix: expect CLEAN (7 goals, no multiplication)
      if multiplicationMatches > 0 {
        let msg =
          "BUG DETECTED: doubled markers in document\n" ++
          "goals=" ++ string_of_int(goals->Array.length) ++ "\n" ++
          "actualHoleRanges=" ++ string_of_int(actualHoleRanges->Array.length) ++ "\n" ++
          "multiplicationMatches=" ++ string_of_int(multiplicationMatches)
        Assert.fail(msg)
      }
      if duplicateRanges->Array.length > 0 {
        let msg =
          "BUG DETECTED: duplicate goal ranges\n" ++
          "goals=" ++ string_of_int(goals->Array.length) ++ "\n" ++
          "duplicateRanges=" ++ string_of_int(duplicateRanges->Array.length)
        Assert.fail(msg)
      }
      if actualHoleRanges->Array.length != 7 || goals->Array.length != 7 {
        let msg =
          "BUG OR LOSS: goal/hole count mismatch\n" ++
          "goals=" ++ string_of_int(goals->Array.length) ++ " (expected 7)\n" ++
          "actualHoleRanges=" ++ string_of_int(actualHoleRanges->Array.length) ++ " (expected 7)"
        Assert.fail(msg)
      }
      // If we reach here, the test passed: goals settled cleanly to 7, no multiplication
    },
  )
})
