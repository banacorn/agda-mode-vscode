open Mocha
open Test__Util

@module("vscode") @scope("commands")
external executeCommand: string => promise<unit> =
  "executeCommand"

describe_skip("Undo/Redo History", () => {
  This.timeout(4000)
  let filename = "CaseSplit.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("shoule make case split a single transaction", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    // perform a case split
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(8, 11), ~payload="x")
    // perform a undo
    await executeCommand("undo")

    // compare file content before and after
    let actual = await File.read(Path.asset("CaseSplit.agda"))
    let expected = fileContent.contents
    Assert.deepStrictEqual(actual, expected)
  })
})
