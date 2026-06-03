open Mocha
open Test__Util

type setup = {
  editor: VSCode.TextEditor.t,
  channels: State.channels,
}

let acquire = setup =>
  switch setup.contents {
  | None => raise(Exn("Setup is not initialized"))
  | Some(setup) => setup
  }

let cleanup = async setup => {
  let range = VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(100, 0))
  let _ = await setup.editor->VSCode.TextEditor.document->Editor.Text.replace(range, "")
}

module IM = {
  include IM

  let wait = async setup => await setup.channels.inputMethod->Chan.once

  // waits for n number of events
  let waitMany = async (setup, n) => {
    let received = []

    let (promise, resolve, _) = Util.Promise_.pending()
    let destructor = setup.channels.inputMethod->Chan.on(batch => {
      received->Array.push(batch) // mutates the array
      if received->Array.length == n {
        resolve()
      }
    })

    await promise
    destructor()
    Array.flat(received)
  }

  let activate = async (setup, ~positions=?, ()) => {
    let promise = waitMany(setup, 1)
    let positions = positions->Option.getOr(Editor.Cursor.getMany(setup.editor))
    Editor.Cursor.setMany(setup.editor, positions)
    let _ = await VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
    await promise
  }

  let deactivate = async setup => {
    let promise = waitMany(setup, 1)
    let result = VSCode.Commands.executeCommand0("agda-mode.escape")
    let _ = await result
    await promise
  }

  let insertChar = async (setup, char) => {
    let promise = waitMany(setup, 2)

    let positions = Editor.Cursor.getMany(setup.editor)

    let succeed =
      await setup.editor
      ->VSCode.TextEditor.document
      ->Editor.Text.batchInsert(positions, char)

    if succeed {
      await promise
    } else {
      raise(Js.Exn.raiseError("Failed to insert " ++ char))
    }
  }

  // pressing the right arrow key
  let rightArrow = async setup => {
    let promise = waitMany(setup, 2)
    let _ = await VSCode.Commands.executeCommand0("agda-mode.input-symbol[BrowseRight]")
    await promise
  }

  // pressing the left arrow key
  let leftArrow = async setup => {
    let promise = waitMany(setup, 2)
    let _ = await VSCode.Commands.executeCommand0("agda-mode.input-symbol[BrowseLeft]")
    await promise
  }

  // pressing the up arrow key
  let upArrow = async setup => {
    let promise = waitMany(setup, 2)
    let _ = await VSCode.Commands.executeCommand0("agda-mode.input-symbol[BrowseUp]")
    await promise
  }

  // pressing the down arrow key
  let downArrow = async setup => {
    let promise = waitMany(setup, 2)
    let _ = await VSCode.Commands.executeCommand0("agda-mode.input-symbol[BrowseDown]")
    await promise
  }

  let backspace = async setup => {
    let promise = waitMany(setup, 2)
    let end_ = Editor.Cursor.get(setup.editor)
    let start = end_->VSCode.Position.translate(0, -1)
    let range = VSCode.Range.make(start, end_)
    let succeed =
      await setup.editor
      ->VSCode.TextEditor.document
      ->Editor.Text.delete(range)

    if succeed {
      await promise
    } else {
      raise(Js.Exn.raiseError("Failed to backspace"))
    }
  }

  let select = async (setup, intervals) => {
    let ranges =
      intervals->Array.map(
        Common.Interval.toVSCodeRange(setup.editor->VSCode.TextEditor.document, ...)
      )
    Editor.Selection.setMany(setup.editor, ranges)
  }

  let selectAndWait = async (setup, intervals) => {
    let promise = wait(setup)
    let ranges =
      intervals->Array.map(
        Common.Interval.toVSCodeRange(setup.editor->VSCode.TextEditor.document, ...)
      )
    Editor.Selection.setMany(setup.editor, ranges)
    await promise
  }
}

describe("Input Method (Editor)", () => {
  let setup = ref(None)

  // initialize the setup before all tests
  Async.before(async () => {
    let (editor, channels) = await activateExtensionAndOpenFile(Path.asset("InputMethod.agda"), None)
    setup := Some({editor, channels})
  })

  // cleanup the editor after each test
  Async.afterEach(async () => {
    let setup = acquire(setup)
    await cleanup(setup)
  })

  describe("Insertion", () => {
    Async.it(
      `should translate "lambdabar" to "λ"`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual(log, [IM.Log.Activate])
        let log = await IM.insertChar(setup, "l")
        Assert.deepStrictEqual(
          log,
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "←")]), UpdateView, RewriteApplied],
        )
        Assert.equal("←", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual(log, [IM.Log.RewriteIssued([]), UpdateView, RewriteApplied])
        Assert.equal("←a", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "m")
        Assert.deepStrictEqual(log, [IM.Log.RewriteIssued([]), UpdateView, RewriteApplied])
        Assert.equal("←am", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "b")
        Assert.deepStrictEqual(log, [IM.Log.RewriteIssued([]), UpdateView, RewriteApplied])
        Assert.equal("←amb", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "d")
        Assert.deepStrictEqual(log, [IM.Log.RewriteIssued([]), UpdateView, RewriteApplied])
        Assert.equal("←ambd", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual(
          log,
          [IM.Log.RewriteIssued([(((0, 0), (0, 6)), "λ")]), UpdateView, RewriteApplied],
        )
        Assert.equal("λ", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "b")
        Assert.deepStrictEqual(log, [IM.Log.RewriteIssued([]), UpdateView, RewriteApplied])
        Assert.equal("λb", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual(log, [IM.Log.RewriteIssued([]), UpdateView, RewriteApplied])
        Assert.equal("λba", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "r")
        Assert.deepStrictEqual(
          log,
          [IM.Log.RewriteIssued([(((0, 0), (0, 4)), "ƛ")]), Deactivate, RewriteApplied],
        )
        Assert.equal("ƛ", document->Editor.Text.getAll)
      },
    )

    Async.it(
      `should translate "bn" to "𝕟"`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "b")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "♭")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("♭", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "n")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 2)), "𝕟")]), Deactivate, RewriteApplied],
          log,
        )
        Assert.equal("𝕟", document->Editor.Text.getAll)
      },
    )

    Async.it(
      `should translate "\\" to "\\"`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "\\")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "\\")]), Deactivate, RewriteApplied],
          log,
        )
        Assert.equal("\\", document->Editor.Text.getAll)
      },
    )

    Async.it(
      `Issue #55, should not deactivate when size of candidate symbols > 1`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("a", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "s")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("as", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "t")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 3)), "∗")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("∗", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "e")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("∗e", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "r")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("∗er", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "i")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("∗eri", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "s")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("∗eris", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "k")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 6)), "⁎")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("⁎", document->Editor.Text.getAll)
        let log = await IM.deactivate(setup)
        Assert.deepStrictEqual([IM.Log.Deactivate], log)
        Assert.equal("⁎", document->Editor.Text.getAll)
      },
    )

    Async.it(
      `Issue #234, should filter newline preview while more input is possible`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "p")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("p", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 2)), "▰")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("▰", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "r")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("▰r", document->Editor.Text.getAll)
        let _ = await IM.insertChar(setup, "t")
        let _ = await IM.insertChar(setup, "i")
        let _ = await IM.insertChar(setup, "a")
        let log = await IM.insertChar(setup, "l")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 6)), "∂")]), Deactivate, RewriteApplied],
          log,
        )
        Assert.equal("∂", document->Editor.Text.getAll)
      },
    )

    Async.it(
      `Issue #234, should keep leaf newline sequence`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let replaceCRLF = x => x->String.replaceRegExp(%re("/\r\n/g"), "\n") // RegEx updated to v10.1.4
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let _ = await IM.insertChar(setup, "n")
        let _ = await IM.insertChar(setup, "e")
        let _ = await IM.insertChar(setup, "w")
        let _ = await IM.insertChar(setup, "l")
        let _ = await IM.insertChar(setup, "i")
        let _ = await IM.insertChar(setup, "n")
        let log = await IM.insertChar(setup, "e")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 6)), "\n")]), Deactivate, RewriteApplied],
          log,
        )
        Assert.equal("\n", document->Editor.Text.getAll->replaceCRLF)
      },
    )
  })

  describe("Backspacing", () => {
    Async.it(
      `should work just fine`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "l")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "←")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("←", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("←a", document->Editor.Text.getAll)
        let log = await IM.backspace(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "←")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("←", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("←a", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "m")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("←am", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "b")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("←amb", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "d")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("←ambd", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 6)), "λ")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("λ", document->Editor.Text.getAll)
        let log = await IM.backspace(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 0)), "lambd")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("lambd", document->Editor.Text.getAll)
        let log = await IM.deactivate(setup)
        Assert.deepStrictEqual([IM.Log.Deactivate], log)
        Assert.equal("lambd", document->Editor.Text.getAll)
      },
    )
  })

  describe("Abortion", () => {
    Async.it(
      `should abort after hitting escape`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "b")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "♭")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("♭", document->Editor.Text.getAll)
        let log = await IM.deactivate(setup)
        Assert.deepStrictEqual([IM.Log.Deactivate], log)
        Assert.equal("♭", document->Editor.Text.getAll)
      },
    )

    Async.it(
      `should abort after typing the wrong sequence`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("a", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "d")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), Deactivate, RewriteApplied], log)
        Assert.equal("ad", document->Editor.Text.getAll)
      },
    )

    Async.it(
      `should abort after backspacing to much`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("a", document->Editor.Text.getAll)
        let log = await IM.backspace(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 0)), "")]), Deactivate, RewriteApplied],
          log,
        )
        Assert.equal("", document->Editor.Text.getAll)
      },
    )
  })

  describe("Cursor", () => {
    Async.it(
      `should not abort when the cursor is placed inside the buffer`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("a", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "n")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("an", document->Editor.Text.getAll)
        // messing with the cursor
        await IM.select(setup, [(0, 0)])
        await IM.select(setup, [(1, 1)])
        await IM.select(setup, [(2, 2)])
        await IM.select(setup, [(0, 1), (1, 2)])
        await IM.select(setup, [(0, 2)])
        // resume insertion
        let log = await IM.insertChar(setup, "d")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 3)), "∧")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("∧", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "=")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 2)), "≙")]), Deactivate, RewriteApplied],
          log,
        )
        Assert.equal("≙", document->Editor.Text.getAll)
      },
    )

    Async.it(
      `should abort when the cursor is placed outside the buffer`,
      async () => {
        let setup = acquire(setup)
        let positions = [VSCode.Position.make(0, 3)]
        let document = setup.editor->VSCode.TextEditor.document
        let _ = await document->Editor.Text.insert(VSCode.Position.make(0, 0), "123")
        let log = await IM.activate(setup, ~positions, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("123a", document->Editor.Text.getAll)
        let log = await IM.insertChar(setup, "n")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), UpdateView, RewriteApplied], log)
        Assert.equal("123an", document->Editor.Text.getAll)
        // messing with the cursor
        let log = await IM.selectAndWait(setup, [(1, 1)])
        Assert.deepStrictEqual([IM.Log.Deactivate], log)
      },
    )
  })

  describe("Candidates", () => {
    Async.it(
      `should be able to select the correct candidate with arrow keys 1`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        // activate the input method
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        // insert "("
        let log = await IM.insertChar(setup, "(")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "(")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("(", document->Editor.Text.getAll)
        // right arrow
        let log = await IM.rightArrow(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "[")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("[", document->Editor.Text.getAll)
        // down arrow
        let log = await IM.downArrow(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "⟪")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("⟪", document->Editor.Text.getAll)
        // left arrow
        let log = await IM.leftArrow(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "⟨")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("⟨", document->Editor.Text.getAll)
        // up arrow
        let log = await IM.upArrow(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "(")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("(", document->Editor.Text.getAll)
        // deactivate
        let log = await IM.deactivate(setup)
        Assert.deepStrictEqual([IM.Log.Deactivate], log)
        Assert.equal("(", document->Editor.Text.getAll)
      },
    )

    Async.it(
      `should be able to select the correct candidate with arrow keys 2`,
      async () => {
        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        // activate the input method
        let log = await IM.activate(setup, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        // insert "!"
        let log = await IM.insertChar(setup, "!")
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "！")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("！", document->Editor.Text.getAll)
        // right arrow
        let log = await IM.rightArrow(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "¡")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("¡", document->Editor.Text.getAll)
        // up arrow
        let log = await IM.upArrow(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "！")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("！", document->Editor.Text.getAll)
        // down arrow
        let log = await IM.downArrow(setup)
        Assert.deepStrictEqual(
          [IM.Log.RewriteIssued([(((0, 0), (0, 1)), "¡")]), UpdateView, RewriteApplied],
          log,
        )
        Assert.equal("¡", document->Editor.Text.getAll)
        // deactivate
        let log = await IM.deactivate(setup)
        Assert.deepStrictEqual([IM.Log.Deactivate], log)
        Assert.equal("¡", document->Editor.Text.getAll)
      },
    )
  })

  describe("Multiple cursors at once", () => {
    let replaceCRLF = x => x->String.replaceRegExp(%re("/\r\n/g"), "\n") // RegEx updated to v10.1.4

    Async.it(
      `should work just fine (𝕟)`,
      async () => {
        let positions = [
          VSCode.Position.make(0, 0),
          VSCode.Position.make(1, 0),
          VSCode.Position.make(2, 0),
          VSCode.Position.make(3, 0),
        ]

        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let _ = await document->Editor.Text.insert(VSCode.Position.make(0, 0), "\n\n\n")
        let log = await IM.activate(setup, ~positions, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "b")
        Assert.deepStrictEqual(
          [
            IM.Log.RewriteIssued([
              (((0, 0), (0, 1)), "♭"),
              (((1, 0), (1, 1)), "♭"),
              (((2, 0), (2, 1)), "♭"),
              (((3, 0), (3, 1)), "♭"),
              // ((2, 3), "♭"),
              // ((4, 5), "♭"),
              // ((6, 7), "♭"),
            ]),
            IM.Log.UpdateView,
            IM.Log.RewriteApplied,
          ],
          log,
        )
        Assert.equal("♭\n♭\n♭\n♭", Editor.Text.getAll(document)->replaceCRLF)
        let log = await IM.insertChar(setup, "n")
        Assert.deepStrictEqual(
          [
            IM.Log.RewriteIssued([
              (((0, 0), (0, 2)), "𝕟"),
              (((1, 0), (1, 2)), "𝕟"),
              (((2, 0), (2, 2)), "𝕟"),
              (((3, 0), (3, 2)), "𝕟"),
              // (((0, 0), (0, 2)), "𝕟"),
              // ((3, 5), "𝕟"),
              // ((6, 8), "𝕟"),
              // ((9, 11), "𝕟"),
            ]),
            IM.Log.Deactivate,
            IM.Log.RewriteApplied,
          ],
          log,
        )
        Assert.equal("𝕟\n𝕟\n𝕟\n𝕟", Editor.Text.getAll(document)->replaceCRLF)
      },
    )

    Async.it(
      `should work just fine (∧)`,
      async () => {
        let positions = [
          VSCode.Position.make(0, 0),
          VSCode.Position.make(1, 1),
          VSCode.Position.make(2, 2),
          VSCode.Position.make(3, 3),
        ]

        let setup = acquire(setup)
        let document = setup.editor->VSCode.TextEditor.document
        let _ = await document->Editor.Text.insert(VSCode.Position.make(0, 0), "123\n123\n123\n123")
        let log = await IM.activate(setup, ~positions, ())
        Assert.deepStrictEqual([IM.Log.Activate], log)
        let log = await IM.insertChar(setup, "a")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), IM.Log.UpdateView, IM.Log.RewriteApplied], log)
        Assert.equal("a123\n1a23\n12a3\n123a", Editor.Text.getAll(document)->replaceCRLF)
        let log = await IM.insertChar(setup, "n")
        Assert.deepStrictEqual([IM.Log.RewriteIssued([]), IM.Log.UpdateView, IM.Log.RewriteApplied], log)
        Assert.equal("an123\n1an23\n12an3\n123an", Editor.Text.getAll(document)->replaceCRLF)
        let log = await IM.insertChar(setup, "d")
        Assert.deepStrictEqual(
          [
            IM.Log.RewriteIssued([
              (((0, 0), (0, 3)), "∧"),
              (((1, 1), (1, 4)), "∧"),
              (((2, 2), (2, 5)), "∧"),
              (((3, 3), (3, 6)), "∧"),
              // (((0, 0), (0, 3)), "∧"),
              // ((8, 11), "∧"),
              // ((16, 19), "∧"),
              // ((24, 27), "∧"),
            ]),
            IM.Log.UpdateView,
            IM.Log.RewriteApplied,
          ],
          log,
        )
        Assert.equal("∧123\n1∧23\n12∧3\n123∧", Editor.Text.getAll(document)->replaceCRLF)
      },
    )
  })
})

describe("Input Method — deltas growth regression", () => {
  let setup = ref(None)

  Async.before(async () => {
    let (editor, channels) = await activateExtensionAndOpenFile(Path.asset("InputMethod.agda"), None)
    setup := Some({editor, channels})
  })

  Async.afterEach(async () => {
    let setup = acquire(setup)
    await cleanup(setup)
  })

  Async.it(
    "deltas length should not grow across IM uses",
    async () => {
      let setup = acquire(setup)

      // Activate the IM and capture the State so we can read deltasLength.
      // Subsequent sessions use IM.activate which reuses the same state.
      let imActivatePromise = IM.waitMany(setup, 1)
      let stateResult = await executeCommand("agda-mode.input-symbol[Activate]")
      let state = switch stateResult {
      | Some(Ok(state)) => state
      | _ => raise(Js.Exn.raiseError("Could not get State.t from activate command"))
      }
      let _ = await imActivatePromise

      // Session 1 (IM already active from above): "bn" → 𝕟, auto-deactivates
      let _ = await IM.insertChar(setup, "b")
      let _ = await IM.insertChar(setup, "n")
      let l1 = state.tokens->Tokens.deltasLength

      // Session 2
      let _ = await IM.activate(setup, ())
      let _ = await IM.insertChar(setup, "b")
      let _ = await IM.insertChar(setup, "n")
      let l2 = state.tokens->Tokens.deltasLength

      // Session 3
      let _ = await IM.activate(setup, ())
      let _ = await IM.insertChar(setup, "b")
      let _ = await IM.insertChar(setup, "n")
      let l3 = state.tokens->Tokens.deltasLength

      // Session 4
      let _ = await IM.activate(setup, ())
      let _ = await IM.insertChar(setup, "b")
      let _ = await IM.insertChar(setup, "n")
      let l4 = state.tokens->Tokens.deltasLength

      // Session 5
      let _ = await IM.activate(setup, ())
      let _ = await IM.insertChar(setup, "b")
      let _ = await IM.insertChar(setup, "n")
      let l5 = state.tokens->Tokens.deltasLength

      // Every session types the same two characters and produces the same symbol.
      // deltasLength should be identical after each one.
      // In practice it keeps climbing, demonstrating the progressive slowdown.
      Assert.deepStrictEqual([l1, l2, l3, l4, l5], [l1, l1, l1, l1, l1])
    },
  )
})
