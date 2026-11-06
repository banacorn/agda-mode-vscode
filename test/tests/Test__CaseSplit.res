open Mocha
open Test__Util

describe("agda-mode.case", () => {
  This.timeout(4000)
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("CaseSplit.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("CaseSplit.agda"), fileContent.contents))

  Async.it("should handle both MakeCase::ExtendedLambda and MakeCase::Function", async () => {
    let ctx = await AgdaMode.makeAndLoad("CaseSplit.agda")
    // 6 goals before case splitting
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 6)

    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(8, 11), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 7)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(13, 16), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 8)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(18, 11), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 9)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(23, 20), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 10)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(28, 9), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 11)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(32, 21), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 12)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(9, 13), ~payload="y")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 13)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(15, 20), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 14)

    // compare file content before and after
    let actual = await File.read(Path.asset("CaseSplit.agda"))
    let expected = await File.read(Path.asset("CaseSplit.agda.out"))
    Assert.deepStrictEqual(actual, expected)
  })
})

// Agda sends the whole clause, including the code after the hole, but neither
// a comment nor a `where`. `split b = {! !}` in the fixture is replaced by the
// clause under test, and each case asserts the whole document after the split.
describe("agda-mode.case with text after the hole", () => {
  This.timeout(10000)
  let filename = "CaseSplitAfterHole.agda"
  let placeholder = "split b = {! !}"
  // A checkout can have CRLF, and VS Code keeps the line endings of an open document
  // whatever is written to it, so the table below is compared with LF
  let withLF = text => text->String.replaceAll("\r\n", "\n")
  let fixture = ref("")

  Async.beforeEach(async () => fixture := (await File.read(Path.asset(filename)))->withLF)
  Async.afterEach(async () => {
    await Registry.removeAndDestroyAll()
    await File.write(Path.asset(filename), fixture.contents)
  })

  let splitFirstHole = async (ctx: AgdaMode.t, ~payload) => {
    let document = ctx.state.document
    let offset = Editor.Text.getAll(document)->String.indexOf("{!") + 2
    await AgdaMode.case(ctx, ~cursor=VSCode.TextDocument.positionAt(document, offset), ~payload)
  }

  let cases = [
    (
      "should replace a closing parenthesis after the hole",
      "split b = S ({! !})",
      "b",
      "split (box x) = S ({!   !})\nsplit nox = S ({!   !})",
    ),
    (
      "should replace an expression after the hole",
      "split b = S ({! !} + Z)",
      "b",
      "split (box x) = S ({!   !} + Z)\nsplit nox = S ({!   !} + Z)",
    ),
    (
      "should replace the code after a hole spanning two lines",
      "split b = S ({!\n  !})",
      "b",
      "split (box x) = S ({!   !})\nsplit nox = S ({!   !})",
    ),
    (
      "should replace the code before a line comment and keep the comment",
      "split b = S ({! !}) -- keep me",
      "b",
      "split (box x) = S ({!   !})\nsplit nox = S ({!   !}) -- keep me",
    ),
    (
      "should replace the code before a `where` and keep the `where`",
      "split b = S ({! !}) where\n  y = Z",
      "b",
      "split (box x) = S ({!   !})\nsplit nox = S ({!   !}) where\n  y = Z",
    ),
    (
      "should not read `--` inside a name as a comment",
      "split b = S ({! !} + a--b)",
      "b",
      "split (box x) = S ({!   !} + a--b)\nsplit nox = S ({!   !} + a--b)",
    ),
    (
      "should not read `where` inside a name as the keyword",
      "split b = S ({! !} + whereish)",
      "b",
      "split (box x) = S ({!   !} + whereish)\nsplit nox = S ({!   !} + whereish)",
    ),
    (
      "should not read `--` inside a string as a comment",
      "split b = S ({! !} + (\"a -- b\" ++ Z))",
      "b",
      "split (box x) = S ({!   !} + (\"a -- b\" ++ Z))\nsplit nox = S ({!   !} + (\"a -- b\" ++ Z))",
    ),
    (
      "should replace code with a non-BMP character after the hole",
      "split b = S ({! !} + 𝐵)",
      "b",
      "split (box x) = S ({!   !} + 𝐵)\nsplit nox = S ({!   !} + 𝐵)",
    ),
    (
      "should not duplicate a later hole on the line",
      "split b = {! !} + {! Z !}",
      "b",
      "split (box x) = {!   !} + {!   !}\nsplit nox = {!   !} + {!   !}",
    ),
    (
      "should replace the code after the hole in an indented clause",
      "split b = Z where\n  g : Box → ℕ\n  g c = S ({! !})",
      "c",
      "split b = Z where\n  g : Box → ℕ\n  g (box x) = S ({!   !})\n  g nox = S ({!   !})",
    ),
    (
      "should replace the code after the hole in a `with` clause",
      "split b with Z\n... | z = S ({! !} + z)",
      "b",
      "split b with Z\nsplit (box x) | z = S ({!   !} + z)\nsplit nox | z = S ({!   !} + z)",
    ),
    (
      "should keep a line comment after the hole",
      "split b = S {! !} -- keep me",
      "b",
      "split (box x) = S {!   !}\nsplit nox = S {!   !} -- keep me",
    ),
    (
      "should keep a `where` after the hole",
      "split b = S {! !} where\n  y = Z",
      "b",
      "split (box x) = S {!   !}\nsplit nox = S {!   !} where\n  y = Z",
    ),
    (
      "should keep a block comment after the hole",
      "split b = S {! !} {- block -}",
      "b",
      "split (box x) = S {!   !}\nsplit nox = S {!   !} {- block -}",
    ),
    (
      "should keep a declaration after a `;`",
      "k : ℕ\nsplit b = {! !} ; k = Z",
      "b",
      "k : ℕ\nsplit (box x) = {!   !}\nsplit nox = {!   !} ; k = Z",
    ),
    (
      "should replace a `;` after a hole inside braces",
      "split b = Z\n\nr : Box → R\nr b = record { a = {! !} ; c = Z }",
      "b",
      "split b = Z\n\nr : Box → R\nr (box x) = record { a = {!   !} ; c = Z }\nr nox = record { a = {!   !} ; c = Z }",
    ),
    (
      "should not count a brace in a block comment before the hole",
      "split b = Z\n\nr : Box → R\nr b = record { a = {- } -} {! !} ; c = Z }",
      "b",
      "split b = Z\n\nr : Box → R\nr (box x) = record { a = {!   !} ; c = Z }\nr nox = record { a = {!   !} ; c = Z }",
    ),
    (
      "should keep a declaration after a `;` after a hole inside braces",
      "k : ℕ\nsplit b = h {n = {! !}} ; k = Z",
      "b",
      "k : ℕ\nsplit (box x) = h {n = {!   !}}\nsplit nox = h {n = {!   !}} ; k = Z",
    ),
    (
      "should not read a `;` in a character literal as a separator",
      "split b = S ({! !} + ch ';')",
      "b",
      "split (box x) = S ({!   !} + ch ';')\nsplit nox = S ({!   !} + ch ';')",
    ),
    (
      "should not read a `{` in a character literal as a brace",
      "k : ℕ\nsplit b = ch '{' + {! !} ; k = Z",
      "b",
      "k : ℕ\nsplit (box x) = ch '{' + {!   !}\nsplit nox = ch '{' + {!   !} ; k = Z",
    ),
    (
      "should replace a qualified name after the hole",
      "split b = S ({! !} + R.a r0)",
      "b",
      "split (box x) = S ({!   !} + R.a r0)\nsplit nox = S ({!   !} + R.a r0)",
    ),
    (
      "should count `{{` as two braces",
      "k : ℕ\nsplit b = inst {{n = {! !}}} ; k = Z",
      "b",
      "k : ℕ\nsplit (box x) = inst ⦃ n = {!   !} ⦄\nsplit nox = inst ⦃ n = {!   !} ⦄ ; k = Z",
    ),
    (
      "should keep the `module` of a named `where` block",
      "split b = S {! !} module M where\n  y = Z",
      "b",
      "split (box x) = S {!   !}\nsplit nox = S {!   !} module M where\n  y = Z",
    ),
    (
      "should replace a `;` inside braces",
      "split b = S ({! !} + app (λ { x → x ; y → y }))",
      "b",
      "split (box x) = S ({!   !} + app (λ { x → x ; y → y }))\nsplit nox = S ({!   !} + app (λ { x → x ; y → y }))",
    ),
  ]

  cases->Array.forEach(((name, clause, payload, expected)) => {
    Async.it(name, async () => {
      await File.write(
        Path.asset(filename),
        fixture.contents->String.replace(placeholder, clause),
      )
      let ctx = await AgdaMode.makeAndLoad(filename)
      await splitFirstHole(ctx, ~payload)
      Assert.deepStrictEqual(
        Editor.Text.getAll(ctx.state.document)->withLF,
        fixture.contents->String.replace(placeholder, expected),
      )
    })
  })

  // Agda does not know text typed after the load, so it does not reprint that text
  Async.it("should keep text typed after the load", async () => {
    await File.write(
      Path.asset(filename),
      fixture.contents->String.replace(placeholder, "split b = S ({! !}) "),
    )
    let ctx = await AgdaMode.makeAndLoad(filename)
    let document = ctx.state.document
    let offset = Editor.Text.getAll(document)->String.indexOf("!}) ") + 4
    let _ = await Editor.Text.insert(
      document,
      VSCode.TextDocument.positionAt(document, offset),
      "+ Z",
    )
    await splitFirstHole(ctx, ~payload="b")
    Assert.deepStrictEqual(
      Editor.Text.getAll(ctx.state.document)->withLF,
      fixture.contents->String.replace(
        placeholder,
        "split (box x) = S ({!   !})\nsplit nox = S ({!   !}) + Z",
      ),
    )
  })

  // VS Code keeps the line endings of an open document, so a CRLF document needs
  // its own file, written to disk before VS Code opens it
  describe("in a CRLF document", () => {
    let crlfFilename = "CaseSplitAfterHoleCRLF.agda"
    let toCRLF = text =>
      text
      ->String.replace("module CaseSplitAfterHole where", "module CaseSplitAfterHoleCRLF where")
      ->String.replaceAll("\n", "\r\n")

    Async.afterEach(async () => {
      await Registry.removeAndDestroyAll()
      NodeJs.Fs.unlinkSync(Path.asset(crlfFilename))
    })

    Async.it("should replace a closing parenthesis after the hole", async () => {
      NodeJs.Fs.writeFileSync(
        Path.asset(crlfFilename),
        NodeJs.Buffer.fromString(
          fixture.contents->String.replace(placeholder, "split b = S ({! !})")->toCRLF,
        ),
      )
      let ctx = await AgdaMode.makeAndLoad(crlfFilename)
      await splitFirstHole(ctx, ~payload="b")
      Assert.deepStrictEqual(
        Editor.Text.getAll(ctx.state.document),
        fixture.contents
        ->String.replace(placeholder, "split (box x) = S ({!   !})\nsplit nox = S ({!   !})")
        ->toCRLF,
      )
    })
  })
})

// #335: after a case split, the first hole can end up with no `0` decoration.
//
// A case split writes `?` for each new hole and reloads. `scanAllGoals` is what
// widens each `?` into `{!   !}` and re-ranges its goal; a goal whose range is
// still 1 character wide is given no decoration at all
// (`InternalGoal.make`, `src/Goals.res:172`). The widening is skipped whenever
// the scanner is already busy (`src/Goals.res:844`), so a document change that
// arrives inside that window can leave a `?` unwidened — and its goal
// undecorated.
//
//   AGDA_TEST_GLOB="Test__CaseSplit*.js" npm test
describe("issue #335: missing goal decoration after a case split", () => {
  This.timeout(20000)

  let filename = "Issue335.agda"
  let fileContent = ref("")

  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => {
    await File.write(Path.asset(filename), fileContent.contents)
    await Registry.removeAndDestroyAll()
  })

  // Both goals decorated is both goals wider than one character.
  let undecoratedGoals = (goals, document) =>
    goals
    ->Goals.serializeGoals
    ->Array.filterWithIndex((_, i) =>
      switch Goals.getGoalPositionByIndex(goals, i) {
      | Some(start, end) => end - start == 1
      | None => false
      }
    )
    ->Array.map(serialized => (
      serialized,
      switch Goals.getGoalPositionByIndex(goals, 0) {
      | Some(start, end) =>
        Editor.Text.get(
          document,
          VSCode.Range.make(
            VSCode.TextDocument.positionAt(document, start),
            VSCode.TextDocument.positionAt(document, end),
          ),
        )
      | None => ""
      }
    ))

  Async.it("undisturbed: both new goals are widened and decorated", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(19, 20), ~payload="m")

    Assert.deepStrictEqual(
      Goals.serializeGoals(ctx.state.goals),
      ["#0 [20:21-28)", "#1 [21:24-31)"],
    )
    Assert.deepStrictEqual(undecoratedGoals(ctx.state.goals, ctx.state.document), [])
  })

  Async.it("an edit racing the widening leaves a goal undecorated", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)

    // `InteractionPoints` is the response that registers the new goals and then
    // widens each `?` into `{!   !}`. Starting the handler without awaiting it
    // puts an edit of ours in flight against that widening, which is what a
    // keystroke lands as on a machine slow enough to type on. The edit is
    // appended past every goal, so nothing it does shifts them.
    // The `Case` request answers with `InteractionPoints` of its own, before
    // `MakeCase`. The one that matters is the reload's, after it.
    let sawMakeCase = ref(false)
    let raced = ref(false)
    ctx.state.middlewares
    ->Array.push(handler => async response => {
      switch response {
      | Response.MakeCase(_, _) =>
        sawMakeCase := true
        await handler(response)
      | Response.InteractionPoints(_) if sawMakeCase.contents && !raced.contents =>
        raced := true
        let handling = handler(response)
        let lastLine = VSCode.TextDocument.lineCount(ctx.state.document)
        let _ = await Editor.Text.insert(
          ctx.state.document,
          VSCode.Position.make(lastLine, 0),
          "-- perturbation\n",
        )
        await handling
      | _ => await handler(response)
      }
    })
    ->ignore

    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(19, 20), ~payload="m")
    ctx.state.middlewares->Array.pop->ignore
    if !raced.contents {
      Assert.fail("the reload's InteractionPoints was never seen, so nothing raced the widening")
    }

    Js.log("goals after case split: " ++ Goals.serializeGoals(ctx.state.goals)->Array.join(", "))
    Js.log(
      "undecorated: " ++
      undecoratedGoals(ctx.state.goals, ctx.state.document)
      ->Array.map(((serialized, text)) => serialized ++ " over " ++ text)
      ->Array.join(", "),
    )

    // A goal one character wide is a goal `InternalGoal.make` gave no
    // decoration to.
    Assert.deepStrictEqual(undecoratedGoals(ctx.state.goals, ctx.state.document), [])
    Assert.deepStrictEqual(
      Goals.serializeGoals(ctx.state.goals),
      ["#0 [20:21-28)", "#1 [21:24-31)"],
    )
    // A `?` left in the source is the widening that went missing.
    let text = Editor.Text.getAll(ctx.state.document)
    Assert.deepStrictEqual(
      text->String.replaceAll("\r", "")->String.split("\n")->Array.sliceToEnd(~start=19)->Array.slice(~start=0, ~end=2),
      ["+-assoc' zero n p = {!   !}", "+-assoc' (suc m) n p = {!   !}"],
    )
  })
})

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

        disposeLog()
      } catch {
      | exn =>
        releaseOnce()
        disposeLog()
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
