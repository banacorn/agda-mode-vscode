open Mocha
open Test__Util

describe("issue #248: rapid loads must not duplicate goal numbers", () => {
  This.timeout(20000)

  Async.it(
    "keeps one live number decoration per goal after two overlapping Load commands",
    async () => await GoalNumberDecorations.withInstalled(async observer => {
      let ctx = await AgdaMode.makeAndLoad("Load.agda")
      let goalsBefore = Goals.serializeGoals(ctx.state.goals)
      let decorationsBefore = observer->GoalNumberDecorations.snapshot

      let loadDispatches = ref(0)
      let interactionPointHits = ref(0)
      let (secondLoadDispatched, signalSecondLoadDispatched, _) = Util.Promise_.pending()
      let (firstInteractionPoint, signalFirstInteractionPoint, _) = Util.Promise_.pending()
      let (releaseFirstInteractionPoint, release, _) = Util.Promise_.pending()

      let stopObservingLogs = ctx.state.channels.log->Chan.on(log => {
        switch log {
        | CommandDispatched(Load) =>
          loadDispatches := loadDispatches.contents + 1
          if loadDispatches.contents == 2 {
            signalSecondLoadDispatched()
          }
        | _ => ()
        }
      })

      ctx.state.middlewares
      ->Array.push(handler => async response => {
        switch response {
        | Response.InteractionPoints(_) =>
          interactionPointHits := interactionPointHits.contents + 1
          if interactionPointHits.contents == 1 {
            signalFirstInteractionPoint()
          }
          await releaseFirstInteractionPoint
          await handler(response)
        | _ => await handler(response)
        }
      })
      ->ignore

      // Match two rapid C-c C-l invocations. The first command is known to be
      // inside its real InteractionPoints handler when the second is sent, so
      // this overlap does not depend on process or scheduler timing.
      let firstLoad = executeCommand("agda-mode.load")
      await firstInteractionPoint
      let secondLoad = executeCommand("agda-mode.load")
      await secondLoadDispatched
      let interactionPointHitsBeforeRelease = interactionPointHits.contents
      release()
      let _ = await Promise.all([firstLoad, secondLoad])

      let actual = (
        loadDispatches.contents,
        interactionPointHitsBeforeRelease,
        interactionPointHits.contents,
        goalsBefore,
        Goals.serializeGoals(ctx.state.goals),
        decorationsBefore,
        observer->GoalNumberDecorations.snapshot,
      )

      stopObservingLogs()
      ctx.state.middlewares->Array.pop->ignore
      await ctx->AgdaMode.quit

      // The reported failure rendered coincident `00` and `11` overlays even
      // though state still contained only goals 0 and 1. Keep all parts of
      // that signature in one assertion so a regression explains itself.
      Assert.deepStrictEqual(
        actual,
        (
          2,
          1,
          2,
          ["#0 [3:5-12)", "#1 [4:5-12)"],
          ["#0 [3:5-12)", "#1 [4:5-12)"],
          ["0", "1"],
          ["0", "1"],
        ),
      )
    }),
  )
})
