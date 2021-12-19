open Belt
open! BsMocha.Mocha
open! Test__Util

describe("Dry run State__Goal.caseSplitAux", () => {
  Q.it("should calculate the infomation needed for case splitting correctly", () =>
    VSCode.Window.showTextDocumentWithUri(
      VSCode.Uri.file(Path.asset("CaseSplit.agda")),
      None,
    )->Promise.flatMap(editor => {
      let document = VSCode.TextEditor.document(editor)
      Goal.makeMany(editor, [0, 1, 2, 3, 4, 5, 6, 7, 8])
      ->Promise.map(goals =>
        goals->Array.map(goal => {
          // convert `rewriteRange` to text in that range because range offsets are different on different OSs
          let (inWhereClause, indentWidth, rewriteRange) = State__Goal.caseSplitAux(document, goal)
          let rewriteRange = VSCode.Range.make(
            VSCode.TextDocument.positionAt(document, fst(rewriteRange)),
            VSCode.TextDocument.positionAt(document, snd(rewriteRange)),
          )
          (inWhereClause, indentWidth, Editor.Text.get(document, rewriteRange))
        })
      )
      ->Promise.map(results => Ok(
        Assert.deep_equal(
          results,
          [
            (false, 9, j`x → {!   !}`),
            (false, 23, j`y → {!   !}`),
            (false, 4, j`x → {!   !}`),
            (false, 4, j`y → {!   !}`),
            (true, 13, j`x → {!   !}`),
            (true, 13, j`y → {!   !}`),
            (true, 2, j`x → {!   !}`),
            (true, 2, j`y → {!   !}`),
            (false, 13, j`x → {!   !}`),
          ],
        ),
      ))
    })
  )
})

describe("Integration test", () => {
  let channels = ref(None)
  let acquire = filepath => {
    switch channels.contents {
    | None => A.fail("Cannot activate the extension")
    | Some(channels) =>
      let (promise, resolve) = Promise.pending()
      let disposable = channels.State__Type.responseHandled->Chan.on(response => {
        switch response {
        | CompleteHighlightingAndMakePromptReappear => resolve()
        | _ => ()
        }
      })
      let filepath = Path.asset(filepath)
      openFile(filepath)
      ->Promise.flatMap(_ => executeCommand("agda-mode.load"))
      ->Promise.flatMap(result =>
        switch result {
        | None => A.fail("Cannot load " ++ filepath)
        | Some(Ok(state)) =>
          promise->Promise.map(() => {
            disposable() // stop listening to responses
            Ok(state)
          })
        | Some(Error(error)) =>
          let (header, body) = Connection.Error.toString(error)
          A.fail(header ++ "\n" ++ body)
        }
      )
      ->Promise.flatMap(_ => executeCommand("agda-mode.case"))
      ->Promise.flatMap(result =>
        switch result {
        | None => A.fail("Cannot case split " ++ filepath)
        | Some(Ok(state)) =>
          promise->Promise.map(() => {
            disposable() // stop listening to responses
            Ok(state)
          })
        | Some(Error(error)) =>
          let (header, body) = Connection.Error.toString(error)
          A.fail(header ++ "\n" ++ body)
        }
      )
    }
  }

  Q.before(() => {
    Config.inTestingMode := true
    Config.Connection.setAgdaVersion("agda")
    ->Promise.flatMap(() => Config.Connection.setUseAgdaLanguageServer(false))
    ->Promise.flatMap(() => Agda.exists("agda"))
    ->Promise.tap(_ => {
      channels := Some(activateExtension())
    })
  })

  Q.it("should calculate the infomation needed for case splitting correctly", () =>
    acquire("CaseSplit.agda")->Promise.mapOk(state => {
      let results = state.goals->Array.map(goal => {
        // convert `rewriteRange` to text in that range because range offsets are different on different OSs
        let (inWhereClause, indentWidth, rewriteRange) = State__Goal.caseSplitAux(
          state.document,
          goal,
        )
        let rewriteRange = VSCode.Range.make(
          VSCode.TextDocument.positionAt(state.document, fst(rewriteRange)),
          VSCode.TextDocument.positionAt(state.document, snd(rewriteRange)),
        )
        (inWhereClause, indentWidth, Editor.Text.get(state.document, rewriteRange))
      })

      Assert.deep_equal(
        results,
        [
          (false, 9, j`x → {!   !}`),
          (false, 23, j`y → {!   !}`),
          (false, 4, j`x → {!   !}`),
          (false, 4, j`y → {!   !}`),
          (true, 13, j`x → {!   !}`),
          (true, 13, j`y → {!   !}`),
          (true, 2, j`x → {!   !}`),
          (true, 2, j`y → {!   !}`),
          (false, 13, j`x → {!   !}`),
        ],
      )
    })
  )
})
