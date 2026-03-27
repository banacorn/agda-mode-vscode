open Mocha

module Candidate = Connection__Candidate

describe("Connection__Candidate", () => {
  describe("make", () => {
    it("should construct bare command names as Command", () => {
      switch Candidate.make("agda") {
      | Command("agda") => ()
      | _ => Assert.fail("Expected Command(\"agda\")")
      }
    })

    it("should trim surrounding whitespace before constructing Command", () => {
      switch Candidate.make("  agda  ") {
      | Command("agda") => ()
      | _ => Assert.fail("Expected Command(\"agda\") after trimming")
      }
    })

    it("should construct absolute file paths as Resource", () => {
      let expected = NodeJs.Path.resolve(["/usr/bin/agda"])->VSCode.Uri.file
      switch Candidate.make("/usr/bin/agda") {
      | Resource(uri) => Assert.deepStrictEqual(uri->VSCode.Uri.toString, expected->VSCode.Uri.toString)
      | _ => Assert.fail("Expected Resource(file:///usr/bin/agda)")
      }
    })

    it("should preserve vscode-* URIs as Resource", () => {
      let raw = "vscode-userdata:/global/als.wasm"
      switch Candidate.make(raw) {
      | Resource(uri) => Assert.deepStrictEqual(uri->VSCode.Uri.toString, raw)
      | _ => Assert.fail("Expected Resource(vscode-userdata:/global/als.wasm)")
      }
    })

    it("should classify strings containing whitespace as Resource", () => {
      switch Candidate.make("agda custom build") {
      | Resource(_) => ()
      | _ => Assert.fail("Expected Resource for whitespace-containing candidate")
      }
    })

    it("should classify relative path-like strings as Resource", () => {
      switch Candidate.make("./bin/agda") {
      | Resource(_) => ()
      | _ => Assert.fail("Expected Resource for relative path candidate")
      }
    })

    it("should treat lsp:// input as Resource via filepath parsing", () => {
      switch Candidate.make("lsp://agda-language-server") {
      | Resource(_) => ()
      | _ => Assert.fail("Expected Resource for lsp:// input")
      }
    })
  })

  describe("toString", () => {
    it("should render Command as the bare command name", () => {
      Assert.deepStrictEqual(Candidate.toString(Command("agda")), "agda")
    })

    it("should render Resource as canonical URI string", () => {
      let uri = VSCode.Uri.file("/usr/bin/agda")
      Assert.deepStrictEqual(Candidate.toString(Resource(uri)), uri->VSCode.Uri.toString)
    })
  })

  describe("make -> toString -> make", () => {
    let assertStable = raw => {
      let candidate = Candidate.make(raw)
      let roundTripped = candidate->Candidate.toString->Candidate.make
      Assert.deepStrictEqual(Candidate.equal(candidate, roundTripped), true)
    }

    it("should be stable for Command candidates", () => {
      assertStable("agda")
    })

    it("should be stable for file Resource candidates", () => {
      assertStable("/usr/bin/agda")
    })

    it("should be stable for vscode-userdata Resource candidates", () => {
      assertStable("vscode-userdata:/global/als.wasm")
    })
  })

  describe("equal", () => {
    it("should treat absolute filepath and file:// URI as equal resources", () => {
      let filepath = Candidate.make("/usr/bin/agda")
      let fileUri = Candidate.make("file:///usr/bin/agda")
      Assert.deepStrictEqual(Candidate.equal(filepath, fileUri), true)
    })

    it("should distinguish Command from resolved Resource", () => {
      let command = Candidate.make("agda")
      let resource = Candidate.make("/usr/bin/agda")
      Assert.deepStrictEqual(Candidate.equal(command, resource), false)
    })
  })

  describe("deduplicate", () => {
    it("should deduplicate semantically equal resources while preserving first occurrence", () => {
      let first = Candidate.make("/usr/bin/agda")
      let duplicate = Candidate.make("file:///usr/bin/agda")
      let command = Candidate.make("agda")
      let deduped = Candidate.deduplicate([first, duplicate, command])

      Assert.deepStrictEqual(Array.length(deduped), 2)
      Assert.deepStrictEqual(Candidate.equal(deduped[0]->Option.getExn, first), true)
      Assert.deepStrictEqual(Candidate.equal(deduped[1]->Option.getExn, command), true)
    })
  })

  describe("resolve", () => {
    Async.it("should resolve Command through Platform.findCommand", async () => {
      module MockPlatform = {
        let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
        let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
        let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
        let resolveDownloadChannel = (_target, _) => async (_, _, _) =>
          Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        let download = (_globalStorageUri, _downloadDescriptor) =>
          Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
        let findCommand = (command, ~timeout as _timeout=1000) =>
          switch command {
          | "agda" => Promise.resolve(Ok("/resolved/bin/agda"))
          | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
          }
      }

      let platformDeps: Platform.t = module(MockPlatform)
      switch await Candidate.resolve(platformDeps, Candidate.make("agda")) {
      | Ok({original: Command("agda"), resource}) =>
        let expected = VSCode.Uri.file("/resolved/bin/agda")->VSCode.Uri.toString
        Assert.deepStrictEqual(resource->VSCode.Uri.toString, expected)
      | Error(_) => Assert.fail("Expected command resolution to succeed")
      | _ => Assert.fail("Expected resolved command to preserve original candidate")
      }
    })

    Async.it("should leave Resource unchanged when resolving", async () => {
      module MockPlatform = {
        let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
        let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
        let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
        let resolveDownloadChannel = (_target, _) => async (_, _, _) =>
          Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        let download = (_globalStorageUri, _downloadDescriptor) =>
          Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
        let findCommand = (_command, ~timeout as _timeout=1000) =>
          Promise.resolve(Error(Connection__Command.Error.NotFound))
      }

      let platformDeps: Platform.t = module(MockPlatform)
      let candidate = Candidate.make("file:///usr/bin/agda")
      switch (candidate, await Candidate.resolve(platformDeps, candidate)) {
      | (Resource(expected), Ok({original: Resource(original), resource})) =>
        Assert.deepStrictEqual(resource->VSCode.Uri.toString, expected->VSCode.Uri.toString)
        Assert.deepStrictEqual(original->VSCode.Uri.toString, expected->VSCode.Uri.toString)
      | _ => Assert.fail("Expected Resource candidate to resolve without lookup")
      }
    })

    Async.it("should not consult findCommand when resolving Resource", async () => {
      let findCommandCalls = ref(0)

      module MockPlatform = {
        let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
        let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
        let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
        let resolveDownloadChannel = (_target, _) => async (_, _, _) =>
          Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        let download = (_globalStorageUri, _downloadDescriptor) =>
          Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
        let findCommand = (_command, ~timeout as _timeout=1000) => {
          findCommandCalls := findCommandCalls.contents + 1
          Promise.resolve(Error(Connection__Command.Error.NotFound))
        }
      }

      let platformDeps: Platform.t = module(MockPlatform)
      let candidate = Candidate.make("file:///usr/bin/agda")
      switch await Candidate.resolve(platformDeps, candidate) {
      | Ok(_) => Assert.deepStrictEqual(findCommandCalls.contents, 0)
      | Error(_) => Assert.fail("Expected Resource resolution to bypass findCommand")
      }
    })
  })
})
