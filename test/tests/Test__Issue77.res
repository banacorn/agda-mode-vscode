open Mocha
open Test__Util

// Issue #77: after Give or Refine, the filled text is highlighted with Cmd_highlight, whose
// Field and Constructor tokens are dropped because of an Agda bug
// (see Highlighting__AgdaStaleNameWorkaround).
//
// These fixtures run real Agda, so that a change in what Agda sends fails a test.
// Tests marked as SENTINEL are the ones that fail once Agda stops sending stale names:
// today a stale copy turns a real bound variable into a Field or a Constructor, and it is dropped;
// after an upstream fix the variable comes back as a "variable" token.
// When a SENTINEL fails, revisit the workaround: it is not a regression.
//
// Every case runs twice, on copies of the fixtures written to disk with LF and with CRLF line
// breaks, and expects the same filled text and tokens.

describe("Issue #77: highlighting after Give or Refine, through the stale-name workaround", () => {
  let fixtures = [
    "Issue77Records.agda",
    "Issue77Constructors.agda",
    "Issue77Operators.agda",
    "Issue77Projections.agda",
    "Issue77Coinductive.agda",
    "Issue77Unicode.agda",
  ]

  let lineOf = token =>
    switch String.split(token, ":")->Array.get(0) {
    | Some(prefix) => int_of_string_opt(String.sliceToEnd(prefix, ~start=1))
    | None => None
    }

  // writes a copy of each fixture with the given line breaks, under a name ending with the suffix,
  // and returns a function that runs the command on the goal at the cursor of a copy,
  // and returns the text of lines [from, to) and the tokens on these lines
  let withLineBreaks = (suffix, lineBreak) => {
    let path = filename => String.replace(filename, ".agda", suffix ++ ".agda")
    let content = filename => {
      let moduleName = String.replace(filename, ".agda", "")
      NodeJs.Fs.readFileSync(Path.asset(filename))
      ->NodeJs.Buffer.toString
      ->String.replace("module " ++ moduleName ++ " where", "module " ++ moduleName ++ suffix ++ " where")
      ->String.replaceAll("\r\n", "\n")
      ->String.replaceAll("\n", lineBreak)
    }
    Async.before(async () =>
      fixtures->Array.forEach(filename =>
        NodeJs.Fs.writeFileSync(Path.asset(path(filename)), NodeJs.Buffer.fromString(content(filename)))
      )
    )
    // File.write keeps the line breaks of the open document
    Async.afterEach(async () =>
      for i in 0 to Array.length(fixtures) - 1 {
        let filename = fixtures->Array.getUnsafe(i)
        await File.write(Path.asset(path(filename)), content(filename))
      }
    )
    Async.after(async () =>
      fixtures->Array.forEach(filename => NodeJs.Fs.unlinkSync(Path.asset(path(filename))))
    )

    async (filename, command, ~cursor, ~lines as (from, to)) => {
      let ctx = await AgdaMode.makeAndLoad(path(filename))
      // make sure that the document really has the line breaks under test
      Assert.deepStrictEqual(
        Editor.Text.getAll(ctx.state.document)->String.includes("\r\n"),
        lineBreak == "\r\n",
      )
      await ctx->AgdaMode.execute(command, ~cursor)
      let text =
        Editor.Text.getAll(ctx.state.document)
        ->String.splitByRegExp(%re("/\r?\n/"))
        ->Array.filterMap(line => line)
        ->Array.slice(~start=from, ~end=to)
      let tokens =
        (await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get)
        ->Array.map(Highlighting__SemanticToken.toString)
        ->Array.filter(token =>
          switch lineOf(token) {
          | Some(line) => line >= from && line < to
          | None => false
          }
        )
      (text, tokens)
    }
  }

  [("LF", "\n"), ("CRLF", "\r\n")]->Array.forEach(((name, lineBreak)) =>
    describe("with " ++ name ++ " line breaks", () => {
      let fill = withLineBreaks(name, lineBreak)

      describe("record expressions", () => {
        Async.it("Refine: long field names, split over several lines", async () => {
          let actual = await fill(
            "Issue77Records.agda",
            Refine,
            ~cursor=VSCode.Position.make(28, 9),
            ~lines=(28, 33),
          )
          Assert.deepStrictEqual(
            actual,
            (
              [
                "long = record",
                "  { very-long-field-name-1 = {!   !}",
                "  ; very-long-field-name-2 = {!   !}",
                "  ; very-long-field-name-3 = {!   !}",
                "  }",
              ],
              ["(28:0-4) function [agda]", "(28:7-13) keyword [agda]"],
            ),
          )
        })

        Async.it("Refine: long field names, split over several indented lines", async () => {
          let actual = await fill(
            "Issue77Records.agda",
            Refine,
            ~cursor=VSCode.Position.make(32, 12),
            ~lines=(32, 37),
          )
          Assert.deepStrictEqual(
            actual,
            (
              [
                "  long' = record",
                "    { very-long-field-name-1 = {!   !}",
                "    ; very-long-field-name-2 = {!   !}",
                "    ; very-long-field-name-3 = {!   !}",
                "    }",
              ],
              ["(32:2-7) function [agda]", "(32:10-16) keyword [agda]"],
            ),
          )
        })

        Async.it("Refine: field names of the same length", async () => {
          let actual = await fill(
            "Issue77Records.agda",
            Refine,
            ~cursor=VSCode.Position.make(35, 9),
            ~lines=(35, 37),
          )
          Assert.deepStrictEqual(
            actual,
            (
              [
                "same = record",
                "  { aaaa = {!   !} ; bbbb = {!   !} ; cccc = {!   !} ; dddd = {!   !} ; eeee = {!   !} ; ffff = {!   !} }",
              ],
              ["(35:0-4) function [agda]", "(35:7-13) keyword [agda]"],
            ),
          )
        })

        Async.it("Refine: one-letter field names", async () => {
          let actual = await fill(
            "Issue77Records.agda",
            Refine,
            ~cursor=VSCode.Position.make(38, 8),
            ~lines=(38, 39),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["one = record { a = {!   !} ; b = {!   !} }"],
              ["(38:0-3) function [agda]", "(38:6-12) keyword [agda]"],
            ),
          )
        })

        Async.it("Give: a record expression on one line", async () => {
          let actual = await fill(
            "Issue77Records.agda",
            Give,
            ~cursor=VSCode.Position.make(41, 8),
            ~lines=(41, 42),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["big = record { foo = Set ; bar = Set }"],
              [
                "(41:0-3) function [agda]",
                "(41:6-12) keyword [agda]",
                "(41:21-24) type [primitive, agda]",
                "(41:33-36) type [primitive, agda]",
              ],
            ),
          )
        })

        Async.it("Give: a record expression over several lines", async () => {
          let actual = await fill(
            "Issue77Records.agda",
            Give,
            ~cursor=VSCode.Position.make(44, 9),
            ~lines=(44, 48),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["big' = record", "          { foo = Set", "          ; bar = Set", "          }"],
              [
                "(44:0-4) function [agda]",
                "(44:7-13) keyword [agda]",
                "(45:18-21) type [primitive, agda]",
                "(46:18-21) type [primitive, agda]",
              ],
            ),
          )
        })

        Async.it("Refine: non-BMP field names", async () => {
          let actual = await fill(
            "Issue77Unicode.agda",
            Refine,
            ~cursor=VSCode.Position.make(8, 6),
            ~lines=(8, 9),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["r = record { 𝕗𝕠𝕠 = {!   !} ; 𝕓𝕒𝕣 = {!   !} }"],
              ["(8:0-1) function [agda]", "(8:4-10) keyword [agda]"],
            ),
          )
        })
      })

      describe("constructors", () => {
        Async.it("Refine: an ambiguous constructor", async () => {
          let actual = await fill(
            "Issue77Constructors.agda",
            Refine,
            ~cursor=VSCode.Position.make(18, 12),
            ~lines=(18, 19),
          )
          Assert.deepStrictEqual(
            actual,
            (["refined = con {!   !} {!   !}"], ["(18:0-7) function [agda]"]),
          )
        })

        Async.it("Give: nested ambiguous constructors", async () => {
          let actual = await fill(
            "Issue77Constructors.agda",
            Give,
            ~cursor=VSCode.Position.make(21, 11),
            ~lines=(21, 22),
          )
          Assert.deepStrictEqual(
            actual,
            (["nested = con (con nil nil) nil"], ["(21:0-6) function [agda]"]),
          )
        })

        Async.it("Give: qualified constructors", async () => {
          let actual = await fill(
            "Issue77Constructors.agda",
            Give,
            ~cursor=VSCode.Position.make(24, 14),
            ~lines=(24, 25),
          )
          Assert.deepStrictEqual(
            actual,
            (["qualified = A.con A.nil A.nil"], ["(24:0-9) function [agda]"]),
          )
        })

        Async.it("Give: ambiguous constructors in a goal with no spaces inside its braces", async () => {
          let actual = await fill(
            "Issue77Constructors.agda",
            Give,
            ~cursor=VSCode.Position.make(27, 12),
            ~lines=(27, 28),
          )
          Assert.deepStrictEqual(
            actual,
            (["nospace = con nil nil"], ["(27:0-7) function [agda]"]),
          )
        })

        Async.it("Give: ambiguous constructors over several lines", async () => {
          let actual = await fill(
            "Issue77Constructors.agda",
            Give,
            ~cursor=VSCode.Position.make(30, 14),
            ~lines=(30, 33),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["multiline = con", "                nil", "                nil"],
              ["(30:0-9) function [agda]"],
            ),
          )
        })

        Async.it("Give: SENTINEL, an ambiguous constructor applied to bound variables", async () => {
          let actual = await fill(
            "Issue77Constructors.agda",
            Give,
            ~cursor=VSCode.Position.make(35, 13),
            ~lines=(35, 36),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["vars x y = con x y"],
              // the "x" after "con" (35:15-16) is absorbed by a stale copy of "con" and dropped
              ["(35:0-4) function [agda]", "(35:5-6) variable [agda]", "(35:7-8) variable [agda]", "(35:17-18) variable [agda]"],
            ),
          )
        })

        Async.it("Give: an ambiguous constructor next to a function", async () => {
          let actual = await fill(
            "Issue77Constructors.agda",
            Give,
            ~cursor=VSCode.Position.make(38, 8),
            ~lines=(38, 39),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["fun = con (g nil) nil"],
              ["(38:0-3) function [agda]", "(38:11-12) function [agda]"],
            ),
          )
        })

        Async.it("Refine: an unambiguous constructor", async () => {
          let actual = await fill(
            "Issue77Constructors.agda",
            Refine,
            ~cursor=VSCode.Position.make(41, 11),
            ~lines=(41, 42),
          )
          Assert.deepStrictEqual(
            actual,
            (["number = suc {!   !}"], ["(41:0-6) function [agda]"]),
          )
        })

        Async.it("Give: SENTINEL, an ambiguous coinductive constructor applied to a bound variable", async () => {
          let actual = await fill(
            "Issue77Coinductive.agda",
            Give,
            ~cursor=VSCode.Position.make(15, 8),
            ~lines=(15, 16),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["f s = cons s"],
              // the "s" after "cons" (15:11-12) is absorbed by a stale copy of "cons" and dropped
              ["(15:0-1) function [agda]", "(15:2-3) variable [agda]"],
            ),
          )
        })
      })

      describe("operators", () => {
        Async.it("Give: an ambiguous mixfix constructor", async () => {
          let actual = await fill(
            "Issue77Operators.agda",
            Give,
            ~cursor=VSCode.Position.make(13, 11),
            ~lines=(13, 14),
          )
          Assert.deepStrictEqual(
            actual,
            (["mixfix = ⟨ ⟨ nil ⟩ ⟩"], ["(13:0-6) function [agda]"]),
          )
        })

        Async.it("Give: an ambiguous infix constructor", async () => {
          let actual = await fill(
            "Issue77Operators.agda",
            Give,
            ~cursor=VSCode.Position.make(16, 9),
            ~lines=(16, 17),
          )
          Assert.deepStrictEqual(
            actual,
            (["pair = nil , (nil , nil)"], ["(16:0-4) function [agda]"]),
          )
        })
      })

      describe("projections", () => {
        Async.it("Give: SENTINEL, an ambiguous prefix projection applied to a bound variable", async () => {
          let actual = await fill(
            "Issue77Projections.agda",
            Give,
            ~cursor=VSCode.Position.make(14, 13),
            ~lines=(14, 15),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["prefix r = foo r"],
              // the "r" after "foo" (14:15-16) is absorbed by a stale copy of "foo" and dropped
              ["(14:0-6) function [agda]", "(14:7-8) variable [agda]"],
            ),
          )
        })

        Async.it("Give: an ambiguous postfix projection", async () => {
          let actual = await fill(
            "Issue77Projections.agda",
            Give,
            ~cursor=VSCode.Position.make(17, 14),
            ~lines=(17, 18),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["postfix r = r .foo"],
              ["(17:0-7) function [agda]", "(17:8-9) variable [agda]", "(17:12-13) variable [agda]"],
            ),
          )
        })

        Async.it("Give: a qualified projection", async () => {
          let actual = await fill(
            "Issue77Projections.agda",
            Give,
            ~cursor=VSCode.Position.make(20, 16),
            ~lines=(20, 21),
          )
          Assert.deepStrictEqual(
            actual,
            (
              ["qualified r = R.foo r"],
              ["(20:0-9) function [agda]", "(20:10-11) variable [agda]", "(20:20-21) variable [agda]"],
            ),
          )
        })
      })
    })
  )
})
