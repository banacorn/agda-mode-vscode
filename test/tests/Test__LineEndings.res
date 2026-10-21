open Mocha
open Test__Util

// New goals from Give and Refine are located by offsets computed from the
// inserted text. Those offsets must count line breaks as the document stores
// them, not as the host OS writes them.
//
// Each fixture is written to disk with explicit line endings before VS Code
// opens it, so neither the host OS nor the checkout decides them.
//
//   AGDA_TEST_GLOB="Test__LineEndings.bs.js" npm test
describe("Line endings of the document", () => {
  let lines = [
    "module ModuleName where",
    "",
    "record Long : Set₁ where",
    "  field",
    "    very-long-field-name-1 : Set",
    "    very-long-field-name-2 : Set",
    "    very-long-field-name-3 : Set",
    "",
    "long : Long",
    "long = {!   !}",
    "",
  ]

  let cases = [("LF", "\n", "RefineLF"), ("CRLF", "\r\n", "RefineCRLF")]

  cases->Array.forEach(((name, eol, moduleName)) => {
    describe(name, () => {
      let filename = moduleName ++ ".agda"

      Async.beforeEach(async () => {
        let content =
          lines
          ->Array.map(line => line->String.replace("ModuleName", moduleName))
          ->Array.join(eol)
        NodeJs.Fs.writeFileSync(Path.asset(filename), NodeJs.Buffer.fromString(content))
      })

      Async.afterEach(async () => {
        await Registry.removeAndDestroyAll()
        NodeJs.Fs.unlinkSync(Path.asset(filename))
      })

      Async.it(
        "Refine: a multiline record expression with three new goals",
        async () => {
          let ctx = await AgdaMode.makeAndLoad(filename)
          await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(9, 9))

          Assert.deepStrictEqual(
            Editor.Text.getAll(ctx.state.document),
            [
              "module " ++ moduleName ++ " where",
              "",
              "record Long : Set₁ where",
              "  field",
              "    very-long-field-name-1 : Set",
              "    very-long-field-name-2 : Set",
              "    very-long-field-name-3 : Set",
              "",
              "long : Long",
              "long = record",
              "  { very-long-field-name-1 = {!   !}",
              "  ; very-long-field-name-2 = {!   !}",
              "  ; very-long-field-name-3 = {!   !}",
              "  }",
              "",
            ]->Array.join(eol),
          )
          Assert.deepStrictEqual(
            Goals.serializeGoals(ctx.state.goals),
            ["#1 [11:30-37)", "#2 [12:30-37)", "#3 [13:30-37)"],
          )
        },
      )
    })
  })

  // The hole content is typed with "\n", which VS Code stores as "\r\n"
  let giveLines = [
    "module ModuleName where",
    "",
    "data Bool : Set where",
    "  true false : Bool",
    "",
    "fst : Bool → Bool → Bool",
    "fst a b = a",
    "",
    "data _×_ (A B : Set) : Set where",
    "  _,_ : A → B → A × B",
    "",
    "paren : Bool",
    "paren = fst {!   !} true",
    "",
    "noParen : Bool × Bool",
    "noParen = {!   !}",
    "",
  ]

  let giveCases = [("CRLF", "\r\n", "GiveParenCRLF", "GiveNoParenCRLF")]

  giveCases->Array.forEach(((name, eol, parenModule, noParenModule)) => {
    describe(name ++ " (Give)", () => {
      let written = ref([])

      let write = moduleName => {
        let filename = moduleName ++ ".agda"
        let content =
          giveLines
          ->Array.map(line => line->String.replace("ModuleName", moduleName))
          ->Array.join(eol)
        NodeJs.Fs.writeFileSync(Path.asset(filename), NodeJs.Buffer.fromString(content))
        written := [...written.contents, filename]
        filename
      }

      Async.afterEach(async () => {
        await Registry.removeAndDestroyAll()
        written.contents->Array.forEach(filename => NodeJs.Fs.unlinkSync(Path.asset(filename)))
        written := []
      })

      // Agda wraps the content in parentheses, as the hole is an argument
      Async.it(
        "GiveParen: hole content with a `?` on each of two later lines",
        async () => {
          let ctx = await AgdaMode.makeAndLoad(write(parenModule))
          await ctx->AgdaMode.execute(
            Refine,
            ~payload="fst\n    ?\n    ?",
            ~cursor=VSCode.Position.make(12, 14),
          )

          Assert.deepStrictEqual(
            Editor.Text.getAll(ctx.state.document),
            [
              "module " ++ parenModule ++ " where",
              "",
              "data Bool : Set where",
              "  true false : Bool",
              "",
              "fst : Bool → Bool → Bool",
              "fst a b = a",
              "",
              "data _×_ (A B : Set) : Set where",
              "  _,_ : A → B → A × B",
              "",
              "paren : Bool",
              "paren = fst (fst",
              "    {!   !}",
              "    {!   !}) true",
              "",
              "noParen : Bool × Bool",
              "noParen = {!   !}",
              "",
            ]->Array.join(eol),
          )
          Assert.deepStrictEqual(
            Goals.serializeGoals(ctx.state.goals),
            ["#1 [18:11-18)", "#2 [14:5-12)", "#3 [15:5-12)"],
          )
        },
      )

      Async.it(
        "GiveNoParen: hole content with a `?` on each of two lines",
        async () => {
          let ctx = await AgdaMode.makeAndLoad(write(noParenModule))
          await ctx->AgdaMode.execute(
            Give,
            ~payload="?\n  , ?",
            ~cursor=VSCode.Position.make(15, 12),
          )

          Assert.deepStrictEqual(
            Editor.Text.getAll(ctx.state.document),
            [
              "module " ++ noParenModule ++ " where",
              "",
              "data Bool : Set where",
              "  true false : Bool",
              "",
              "fst : Bool → Bool → Bool",
              "fst a b = a",
              "",
              "data _×_ (A B : Set) : Set where",
              "  _,_ : A → B → A × B",
              "",
              "paren : Bool",
              "paren = fst {!   !} true",
              "",
              "noParen : Bool × Bool",
              "noParen = {!   !}",
              "  , {!   !}",
              "",
            ]->Array.join(eol),
          )
          Assert.deepStrictEqual(
            Goals.serializeGoals(ctx.state.goals),
            ["#0 [13:13-20)", "#2 [16:11-18)", "#3 [17:5-12)"],
          )
        },
      )
    })
  })
})
