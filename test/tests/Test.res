open Mocha

before(() => {
  // for mocking the Config
  Config.inTestingMode := true
})
