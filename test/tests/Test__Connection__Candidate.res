open Mocha

module Candidate = Connection__Candidate

describe("Connection__Candidate", () => {
  let sampleResourcePath = if OS.onUnix { "/usr/bin/agda" } else { "D:\\usr\\bin\\agda.exe" }
  let sampleResourceUri = VSCode.Uri.file(sampleResourcePath)->VSCode.Uri.toString
  let sampleResolvedPath = if OS.onUnix { "/resolved/bin/agda" } else { "D:\\resolved\\bin\\agda.exe" }

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
      let expected = NodeJs.Path.resolve([sampleResourcePath])->VSCode.Uri.file
      switch Candidate.make(sampleResourcePath) {
      | Resource(uri) => Assert.deepStrictEqual(uri->VSCode.Uri.toString, expected->VSCode.Uri.toString)
      | _ => Assert.fail("Expected Resource for absolute file path")
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
      let uri = VSCode.Uri.file(sampleResourcePath)
      Assert.deepStrictEqual(Candidate.toString(Resource(uri)), uri->VSCode.Uri.toString)
    })
  })

  describe("Resolved.toString", () => {
    it("should include command provenance for resolved commands", () => {
      let resolved: Candidate.Resolved.t = {
        original: Candidate.Command("agda"),
        resource: VSCode.Uri.file(sampleResolvedPath),
      }
      Assert.deepStrictEqual(
        Candidate.Resolved.toString(resolved),
        "agda => " ++ (VSCode.Uri.file(sampleResolvedPath)->VSCode.Uri.toString),
      )
    })

    it("should include original resource provenance for resolved resources", () => {
      let uri = VSCode.Uri.parse("vscode-userdata:/global/als.wasm")
      let resolved: Candidate.Resolved.t = {
        original: Candidate.Resource(uri),
        resource: uri,
      }
      Assert.deepStrictEqual(
        Candidate.Resolved.toString(resolved),
        uri->VSCode.Uri.toString ++ " => " ++ uri->VSCode.Uri.toString,
      )
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
      assertStable(sampleResourcePath)
    })

    it("should be stable for vscode-userdata Resource candidates", () => {
      assertStable("vscode-userdata:/global/als.wasm")
    })
  })

  describe("equal", () => {
    it("should treat absolute filepath and file:// URI as equal resources", () => {
      let filepath = Candidate.make(sampleResourcePath)
      let fileUri = Candidate.make(sampleResourceUri)
      Assert.deepStrictEqual(Candidate.equal(filepath, fileUri), true)
    })

    it("should distinguish Command from resolved Resource", () => {
      let command = Candidate.make("agda")
      let resource = Candidate.make(sampleResourcePath)
      Assert.deepStrictEqual(Candidate.equal(command, resource), false)
    })
  })

  describe("deduplicate", () => {
    it("should deduplicate semantically equal resources while preserving first occurrence", () => {
      let first = Candidate.make(sampleResourcePath)
      let duplicate = Candidate.make(sampleResourceUri)
      let command = Candidate.make("agda")
      let deduped = Candidate.deduplicate([first, duplicate, command])

      Assert.deepStrictEqual(Array.length(deduped), 2)
      Assert.deepStrictEqual(Candidate.equal(deduped[0]->Option.getExn, first), true)
      Assert.deepStrictEqual(Candidate.equal(deduped[1]->Option.getExn, command), true)
    })
  })

  describe("isUnderDirectory", () => {
    let desktopStorageRoot =
      "/Users/banacorn/Library/Application Support/Code/User/globalStorage/banacorn.agda-mode"
    let desktopStorageRootEscaped =
      "/Users/banacorn/Library/Application%20Support/Code/User/globalStorage/banacorn.agda-mode"

    it("should return false for Command candidates", () => {
      let directory = VSCode.Uri.file("/tmp/dev-als")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(Candidate.make("agda"), directory), false)
    })

    it("should recognize file resources under a file directory", () => {
      let directory = VSCode.Uri.file("/tmp/dev-als")
      let candidate = Candidate.make("/tmp/dev-als/als")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
    })

    it("should recognize unescaped file:// resources under a file directory", () => {
      let directory = VSCode.Uri.file("/tmp/agda switch/dev-als")
      let candidate = Candidate.make("file:///tmp/agda switch/dev-als/als.wasm")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
    })

    it("should recognize vscode-userdata resources under a vscode-userdata directory", () => {
      let directory = VSCode.Uri.parse("vscode-userdata:/global/dev-als")
      let candidate = Candidate.make("vscode-userdata:/global/dev-als/als.wasm")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
    })

    it("should recognize native desktop file candidates under vscode-userdata managed directories", () => {
      let directory =
        VSCode.Uri.parse(
          "vscode-userdata:" ++ desktopStorageRootEscaped ++ "/dev-als",
        )
      let candidate = Candidate.make(desktopStorageRoot ++ "/dev-als/als")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
    })

    it("should recognize native desktop file:// candidates under vscode-userdata managed directories", () => {
      let directory =
        VSCode.Uri.parse(
          "vscode-userdata:" ++ desktopStorageRootEscaped ++ "/dev-als",
        )
      let candidate = Candidate.make(
        "file://" ++ desktopStorageRoot ++ "/dev-als/als",
      )
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
    })

    it("should recognize native desktop file candidates for wasm artifacts under vscode-userdata managed directories", () => {
      let directory =
        VSCode.Uri.parse(
          "vscode-userdata:" ++ desktopStorageRootEscaped ++ "/dev-als",
        )
      let candidate = Candidate.make(desktopStorageRoot ++ "/dev-als/als.wasm")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
    })

    it("should recognize native desktop file candidates under vscode-userdata dev-als managed directories", () => {
      let directory =
        VSCode.Uri.parse(
          "vscode-userdata:" ++ desktopStorageRootEscaped ++ "/dev-als",
        )
      let candidate = Candidate.make(desktopStorageRoot ++ "/dev-als/als")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
    })

    it("should recognize unescaped native desktop file:// candidates with spaces under vscode-userdata managed directories", () => {
      let directory =
        VSCode.Uri.parse(
          "vscode-userdata:" ++ desktopStorageRootEscaped ++ "/dev-als",
        )
      let candidate = Candidate.make(
        "file://" ++ desktopStorageRoot ++ "/dev-als/als.wasm",
      )
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
    })

    it("should recognize vscode-userdata desktop candidates under native file managed directories", () => {
      let directory = VSCode.Uri.file(desktopStorageRoot ++ "/dev-als")
      let candidate = Candidate.make(
        "vscode-userdata:" ++ desktopStorageRootEscaped ++ "/dev-als/als",
      )
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
    })

    it("should return false when mixed-form resource and directory authorities differ", () => {
      let directory =
        VSCode.Uri.parse(
          "vscode-userdata://workspace" ++ desktopStorageRootEscaped ++ "/dev-als",
        )
      let candidate = Candidate.make(desktopStorageRoot ++ "/dev-als/als")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), false)
    })

    it("should still return false for mixed-form candidates outside the managed desktop directory", () => {
      let directory =
        VSCode.Uri.parse(
          "vscode-userdata:" ++ desktopStorageRootEscaped ++ "/dev-als",
        )
      let candidate = Candidate.make(desktopStorageRoot ++ "/latest-als/als")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), false)
    })

    it("should return false for resources outside the directory", () => {
      let directory = VSCode.Uri.file("/tmp/dev-als")
      let candidate = Candidate.make("/tmp/latest-als/als")
      Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), false)
    })

    it("should treat Windows local paths case-insensitively for drive-letter differences", () => {
      if OS.onUnix {
        ()
      } else {
        let directory =
          VSCode.Uri.file(
            "C:\\Users\\alice\\AppData\\Roaming\\Code\\User\\globalStorage\\banacorn.agda-mode\\releases",
          )
        let candidate =
          Candidate.make(
            "c:\\Users\\alice\\AppData\\Roaming\\Code\\User\\globalStorage\\banacorn.agda-mode\\releases\\dev\\als.exe",
          )
        Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
      }
    })

    it("should match raw Windows file path candidate under file:// directory", () => {
      if OS.onUnix {
        ()
      } else {
        let rootPath =
          "C:\\Users\\alice\\AppData\\Roaming\\Code\\User\\globalStorage\\banacorn.agda-mode\\releases\\dev"
        let directory = VSCode.Uri.file(rootPath)
        let candidate = Candidate.make(rootPath ++ "\\als.exe")
        Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
      }
    })

    it("should match raw Windows file path candidate under vscode-userdata directory", () => {
      if OS.onUnix {
        ()
      } else {
        let rootPath =
          "C:\\Users\\alice\\AppData\\Roaming\\Code\\User\\globalStorage\\banacorn.agda-mode\\releases\\dev"
        let rootPathForVscodeUserdata =
          "/c%3A/Users/alice/AppData/Roaming/Code/User/globalStorage/banacorn.agda-mode/releases/dev"
        let directory = VSCode.Uri.parse("vscode-userdata:" ++ rootPathForVscodeUserdata)
        let candidate = Candidate.make(rootPath ++ "\\als.exe")
        Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
      }
    })

    it("should match Windows file:// candidate under vscode-userdata directory", () => {
      if OS.onUnix {
        ()
      } else {
        let rootPath =
          "C:\\Users\\alice\\AppData\\Roaming\\Code\\User\\globalStorage\\banacorn.agda-mode\\releases\\dev"
        let rootPathForVscodeUserdata =
          "/c%3A/Users/alice/AppData/Roaming/Code/User/globalStorage/banacorn.agda-mode/releases/dev"
        let directory = VSCode.Uri.parse("vscode-userdata:" ++ rootPathForVscodeUserdata)
        let candidate =
          Candidate.make(
            VSCode.Uri.file(rootPath ++ "\\als.exe")->VSCode.Uri.toString,
          )
        Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
      }
    })

    it("should match Windows drive-letter case mismatch across mixed forms", () => {
      if OS.onUnix {
        ()
      } else {
        let directory =
          VSCode.Uri.parse(
            "vscode-userdata:/c%3A/Users/alice/AppData/Roaming/Code/User/globalStorage/banacorn.agda-mode/releases",
          )
        let candidate =
          Candidate.make(
            "file:///C%3A/Users/alice/AppData/Roaming/Code/User/globalStorage/banacorn.agda-mode/releases/dev/als.exe",
          )
        Assert.deepStrictEqual(Candidate.isUnderDirectory(candidate, directory), true)
      }
    })
  })

  describe("resolve", () => {
    Async.it("should resolve Command through Platform.findCommand", async () => {
      let findCommand = (command, ~timeout as _timeout=1000) =>
        switch command {
        | "agda" => Promise.resolve(Ok(sampleResolvedPath))
        | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
        }

      switch await Candidate.resolve(findCommand, Candidate.make("agda")) {
      | Ok({original: Command("agda"), resource}) =>
        let expected = VSCode.Uri.file(sampleResolvedPath)->VSCode.Uri.toString
        Assert.deepStrictEqual(resource->VSCode.Uri.toString, expected)
      | Error(_) => Assert.fail("Expected command resolution to succeed")
      | _ => Assert.fail("Expected resolved command to preserve original candidate")
      }
    })

    Async.it("should leave Resource unchanged when resolving", async () => {
      let findCommand = (_command, ~timeout as _timeout=1000) =>
        Promise.resolve(Error(Connection__Command.Error.NotFound))

      let candidate = Candidate.make(sampleResourceUri)
      switch (candidate, await Candidate.resolve(findCommand, candidate)) {
      | (Resource(expected), Ok({original: Resource(original), resource})) =>
        Assert.deepStrictEqual(resource->VSCode.Uri.toString, expected->VSCode.Uri.toString)
        Assert.deepStrictEqual(original->VSCode.Uri.toString, expected->VSCode.Uri.toString)
      | _ => Assert.fail("Expected Resource candidate to resolve without lookup")
      }
    })

    Async.it("should not consult findCommand when resolving Resource", async () => {
      let findCommandCalls = ref(0)

      let findCommand = (_command, ~timeout as _timeout=1000) => {
        findCommandCalls := findCommandCalls.contents + 1
        Promise.resolve(Error(Connection__Command.Error.NotFound))
      }

      let candidate = Candidate.make(sampleResourceUri)
      switch await Candidate.resolve(findCommand, candidate) {
      | Ok(_) => Assert.deepStrictEqual(findCommandCalls.contents, 0)
      | Error(_) => Assert.fail("Expected Resource resolution to bypass findCommand")
      }
    })
  })
})
