open Mocha

describe("Platform", () => {
  describe("Desktop Platform", () => {
    it("should create desktop platform instance", () => {
      let platform = Platform.makeDesktop()
      // Just test that it can be created without errors
      Assert.ok(true)
    })
  })

  describe("Web Platform", () => {
    it("should create web platform instance", () => {
      let platform = Platform.makeWeb()
      // Just test that it can be created without errors  
      Assert.ok(true)
    })

    it("should have proper interface", () => {
      // Test that Web module has all the required functions
      module PlatformOps = Platform.Web
      
      // Check that functions exist by testing their types
      let _determinePlatform: unit => promise<result<Connection__Download__Platform.t, Connection__Download__Platform.raw>> = PlatformOps.determinePlatform
      let _findCommands: array<string> => promise<result<Connection__Target.t, array<Connection__Command.Error.t>>> = PlatformOps.findCommands
      let _askUserAboutDownloadPolicy: unit => promise<Config.Connection.DownloadPolicy.t> = PlatformOps.askUserAboutDownloadPolicy
      
      Assert.ok(true)
    })
  })

  describe("Platform factory", () => {
    it("should create web platform when isWeb=true", () => {
      let platform = Platform.makePlatform(~isWeb=true)
      // Just test that it can be created without errors
      Assert.ok(true)
    })

    it("should create desktop platform when isWeb=false", () => {
      let platform = Platform.makePlatform(~isWeb=false)
      // Just test that it can be created without errors
      Assert.ok(true)
    })
  })
})