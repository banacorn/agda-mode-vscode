open Mocha

before(() => {
  // for mocking the Config
  Config.inTestingMode := true
})

Async.beforeEach(async () => {
  await Registry__Connection.shutdown()
  // DownloadPolicy is a single shared ref in testing mode (see Config.res), with
  // no per-test isolation. Reset it here so no test leaks its policy into the
  // next one — tests that need a specific policy still set it themselves.
  await Config.Connection.DownloadPolicy.set(Undecided)
})
