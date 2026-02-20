open Mocha

before(() => {
  // for mocking the Config
  Config.inTestingMode := true
})

Async.beforeEach(async () => {
  await Registry__Connection.shutdown()
})
