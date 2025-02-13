open Mocha
open Test__Util

describe("Connection", () => {
  describe("Picked connection", () => {
    Async.it(
      "should return the previously picked connection",
      async () => {
        // let ctx = await AgdaMode.makeAndLoad("Load.agda")

        // setup the memento
        let agdaMockPath = await Target.Agda.mock(~version="2.7.0.1", ~name="agda-mock")
        let agdaMockTarget = switch await Connection.Target.fromRawPath(agdaMockPath) {
        | Ok(target) => target
        | Error(error) =>
          failwith(
            "Got error when trying to construct a mock for Agda:\n" ++
            fst(Connection.Target.Error.toString(error)) ++
            ": " ++
            snd(Connection.Target.Error.toString(error)),
          )
        }

        let memento = State__Memento.make(None)
        await Connection.Target.setPicked(memento, Some(agdaMockTarget))

        let paths = [agdaMockPath, "path/to/als"]

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = Some(agdaMockTarget)

        Assert.deepEqual(actual, expected)
      },
    )
  })
})
