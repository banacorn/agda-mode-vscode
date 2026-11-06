open Mocha
open Test__Util

// describe("State__Goal.caseSplitAux dry run", () => {
//   Async.it("should calculate the information needed for case splitting correctly", async () => {
//     let editor = await VSCode.Window.showTextDocumentWithUri(
//       VSCode.Uri.file(Path.asset("CaseSplit.agda")),
//       None,
//     )
//     let document = VSCode.TextEditor.document(editor)
//     let goals = await Goal.makeMany(editor, [(0, VSCode.Range.make(VSCode.Position.make(7, 14), VSCode.Position.make(7, 21))), 1, 2, 3, 4, 5, 6, 7, 8])
//     let results = goals->Array.map(
//       goal => {
//         // convert `rewriteRange` to text in that range because range offsets are different on different OSs
//         let (inWhereClause, indentWidth, rewriteRange) = State__Goal.caseSplitAux(document, goal)
//         let rewriteRange = VSCode.Range.make(
//           VSCode.TextDocument.positionAt(document, fst(rewriteRange)),
//           VSCode.TextDocument.positionAt(document, snd(rewriteRange)),
//         )
//         (inWhereClause, indentWidth, Editor.Text.get(document, rewriteRange))
//       },
//     )

//     Assert.deepStrictEqual(
//       results,
//       [
//         (false, 9, `x → {!   !}`),
//         (false, 23, `y → {!   !}`),
//         (false, 4, `x → {!   !}`),
//         (false, 4, `y → {!   !}`),
//         (true, 13, `x → {!   !}`),
//         (true, 13, `y → {!   !}`),
//         (true, 2, `x → {!   !}`),
//         (true, 2, `y → {!   !}`),
//         (false, 13, `x → {!   !}`),
//       ],
//     )
//   })
// })

// describe("agda-mode:case", () => {
//   This.timeout(10000)
//   let fileContent = ref("")

//   Async.before(async () => fileContent := (await File.read(Path.asset("CaseSplit.agda"))))
//   Async.after(async () => await File.write(Path.asset("CaseSplit.agda"), fileContent.contents))

//   Async.it("should have more goals after splitting", async () => {
//     let ctx = await AgdaMode.makeAndLoad("CaseSplit.agda")
//     await ctx->AgdaMode.case(Some(VSCode.Position.make(7, 16), "x"))
//     Assert.deepStrictEqual(Array.length(ctx.state.goals), 10)
//   })
// })

describe("agda-mode.case", () => {
  This.timeout(4000)
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("CaseSplit.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("CaseSplit.agda"), fileContent.contents))

  // Async.it("should be responded with correct responses", async () => {
  //   let ctx = await AgdaMode.makeAndLoad("CaseSplit.agda")

  //   let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(8, 11), "x")
  //   let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
  //     Request.Case({
  //       index: 0,
  //       indexString: "0",
  //       start: 181,
  //       end: 188,
  //     }),
  //   )

  //   let filteredResponses = responses->Array.filter(filteredResponse)
  //   Assert.deepStrictEqual(
  //     filteredResponses,
  //     [
  //       // InteractionPoints([0, 1, 2, 3, 4, 5, 6, 7, 8]),
  //       // MakeCase(ExtendedLambda, ["Z → ?", "(S x) → ?"]),
  //     ],
  //   )
  // })

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
