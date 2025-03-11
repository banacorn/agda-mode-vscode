open Mocha

describe("Config", () => {
  describe("Connection", () => {
    describe(
      "Paths",
      () => {
        Async.it(
          "getAgdaPaths . setAgdaPaths = id",
          async () => {
            let expected = []
            await Config.Connection.setAgdaPaths(expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepEqual(actual, expected)

            let expected = ["some/path", "some/other/path"]->Array.map(Connection__URI.parse)
            await Config.Connection.setAgdaPaths(expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepEqual(actual, expected)
          },
        )

        Async.it(
          "`setAgdaPaths` should remove previous paths",
          async () => {
            await Config.Connection.setAgdaPaths(["some/path"]->Array.map(Connection__URI.parse))
            let expected = ["some/other/path"]->Array.map(Connection__URI.parse)
            await Config.Connection.setAgdaPaths(expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepEqual(actual, expected)
          },
        )

        Async.it(
          "`addAgdaPaths` should be idempotent",
          async () => {
            let expected = ["some/path"]->Array.map(Connection__URI.parse)
            await Config.Connection.setAgdaPaths(expected)
            await Config.Connection.setAgdaPaths(expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepEqual(actual, expected)
          },
        )
      },
    )
  })
})
