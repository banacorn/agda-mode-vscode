open Mocha

describe("agda-mode.compile", () => {
  let savedBackend = ref(Config.backendInTestingMode.contents)
  Async.beforeEach(async () => {
    savedBackend := Config.backendInTestingMode.contents
  })
  Async.afterEach(async () => {
    Config.backendInTestingMode := savedBackend.contents
  })

  Async.it("should preserve GHC and GHCNoMain backend settings", async () => {
    Config.backendInTestingMode := "GHC"
    Assert.strictEqual(Config.getBackend(), "GHC")

    Config.backendInTestingMode := "GHCNoMain"
    Assert.strictEqual(Config.getBackend(), "GHCNoMain")
  })
})
