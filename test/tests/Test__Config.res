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

            Assert.deepStrictEqual(actual, expected)

            let expected = ["some/path", "some/other/path"]->Array.map(Connection__URI.parse)
            await Config.Connection.setAgdaPaths(expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepStrictEqual(actual, expected)
          },
        )

        Async.it(
          "`setAgdaPaths` should remove previous paths",
          async () => {
            await Config.Connection.setAgdaPaths(["some/path"]->Array.map(Connection__URI.parse))
            let expected = ["some/other/path"]->Array.map(Connection__URI.parse)
            await Config.Connection.setAgdaPaths(expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepStrictEqual(actual, expected)
          },
        )

        Async.it(
          "`addAgdaPaths` should be idempotent",
          async () => {
            let expected = ["some/path"]->Array.map(Connection__URI.parse)
            await Config.Connection.setAgdaPaths(expected)
            await Config.Connection.setAgdaPaths(expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepStrictEqual(actual, expected)
          },
        )

        // https://github.com/banacorn/agda-mode-vscode/issues/233
        Async.it(
          "`parseAgdaPaths` should be able to handle corrupted data (Issue #233)",
          async () => {
            Assert.deepStrictEqual(Config.Connection.parseAgdaPaths("[]"), [])
            Assert.deepStrictEqual(
              Config.Connection.parseAgdaPaths(
                "[\"/Users/banacorn/.local/bin/agda\", \"/Users/banacorn/Library/Application Support/Code/User/globalStorage/banacorn.agda-mode/latest-als/als\"]",
              ),
              [
                "/Users/banacorn/Library/Application Support/Code/User/globalStorage/banacorn.agda-mode/latest-als/als",
                "/Users/banacorn/.local/bin/agda",
              ],
            )
            Assert.deepStrictEqual(Config.Connection.parseAgdaPaths("{\"enabled\": true}"), [])
          },
        )
      },
    )
  })
})
