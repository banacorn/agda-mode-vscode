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
            await Config.Connection.setAgdaPaths(Chan.make(), expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepStrictEqual(actual, expected)

            let expected = ["some/path", "some/other/path"]
            await Config.Connection.setAgdaPaths(Chan.make(), expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepStrictEqual(actual, expected)
          },
        )

        Async.it(
          "`setAgdaPaths` should remove previous paths",
          async () => {
            await Config.Connection.setAgdaPaths(Chan.make(), ["some/path"])
            let expected = ["some/other/path"]
            await Config.Connection.setAgdaPaths(Chan.make(), expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepStrictEqual(actual, expected)
          },
        )

        Async.it(
          "`addAgdaPaths` should be idempotent",
          async () => {
            let expected = ["some/path"]
            await Config.Connection.setAgdaPaths(Chan.make(), expected)
            await Config.Connection.setAgdaPaths(Chan.make(), expected)
            let actual = Config.Connection.getAgdaPaths()

            Assert.deepStrictEqual(actual, expected)
          },
        )

        // https://github.com/banacorn/agda-mode-vscode/issues/233
        Async.it(
          "`parseAgdaPaths` should be able to handle corrupted data (Issue #233)",
          async () => {
            Assert.deepStrictEqual(Config.Connection.parseAgdaPaths(JSON.Null), [])
            Assert.deepStrictEqual(Config.Connection.parseAgdaPaths(JSON.Array([])), [])
            Assert.deepStrictEqual(
              Config.Connection.parseAgdaPaths(
                JSON.Array([
                  JSON.String("/Users/banacorn/.local/bin/agda"),
                  JSON.String(
                    "/Users/banacorn/Library/Application Support/Code/User/globalStorage/banacorn.agda-mode/latest-als/als",
                  ),
                ]),
              ),
              [
                "/Users/banacorn/Library/Application Support/Code/User/globalStorage/banacorn.agda-mode/latest-als/als",
                "/Users/banacorn/.local/bin/agda",
              ],
            )
            Assert.deepStrictEqual(
              Config.Connection.parseAgdaPaths(
                JSON.Object(Dict.fromArray([("enabled", JSON.Boolean(true))])),
              ),
              [],
            )
          },
        )
      },
    )
  })

  describe("DevMode", () => {
    describe(
      "defaultValue",
      () => {
        it(
          "should be false",
          () => {
            Assert.strictEqual(Config.DevMode.defaultValue, false)
          },
        )
      },
    )

    describe(
      "parseFromConfig",
      () => {
        it(
          "should return default value for None",
          () => {
            let actual = Config.DevMode.parseFromConfig(None)
            Assert.strictEqual(actual, Config.DevMode.defaultValue)
          },
        )

        it(
          "should parse true correctly",
          () => {
            let actual = Config.DevMode.parseFromConfig(Some(JSON.Boolean(true)))
            Assert.strictEqual(actual, true)
          },
        )

        it(
          "should parse false correctly",
          () => {
            let actual = Config.DevMode.parseFromConfig(Some(JSON.Boolean(false)))
            Assert.strictEqual(actual, false)
          },
        )

        it(
          "should handle invalid values by returning default",
          () => {
            let actual = Config.DevMode.parseFromConfig(Some(JSON.String("invalid")))
            Assert.strictEqual(actual, Config.DevMode.defaultValue)
          },
        )

        it(
          "should handle null by returning default",
          () => {
            let actual = Config.DevMode.parseFromConfig(Some(JSON.Null))
            Assert.strictEqual(actual, Config.DevMode.defaultValue)
          },
        )

        it(
          "should handle number by returning default",
          () => {
            let actual = Config.DevMode.parseFromConfig(Some(JSON.Number(42.0)))
            Assert.strictEqual(actual, Config.DevMode.defaultValue)
          },
        )

        it(
          "should handle array by returning default",
          () => {
            let actual = Config.DevMode.parseFromConfig(Some(JSON.Array([])))
            Assert.strictEqual(actual, Config.DevMode.defaultValue)
          },
        )
      },
    )
  })
})
