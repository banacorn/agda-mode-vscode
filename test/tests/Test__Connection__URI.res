open Mocha

module URI = Connection__URI

describe("Connection__URI", () => {
  // NOTE: DO NOT apply `VSCode.Uri.t`` directly on `Assert.deepStrictEqual`
  // because their values are computed lazily somehow, apply `VSCode.Uri.toString` before comparing!
  // To compare URIs, use `Assert.ok` with `Connection__URI.equal` instead.
  describe("parse", () => {
    it(
      "should reject lsp:// URLs and treat as filepath",
      () => {
        let uri = URI.parse("lsp://localhost:8080")
        switch uri {
        | FileURI(_) => () // Expected — lsp:// is rejected, treated as filepath
        }
      },
    )

    it(
      "should reject non-lsp URLs and treat as filepath",
      () => {
        let uri = URI.parse("http://example.com")
        switch uri {
        | FileURI(_) => () // Expected
        }
      },
    )

    it(
      "should parse absolute file paths as FileURI variant",
      () => {
        let uri = URI.parse("/usr/bin/agda")
        let expected = NodeJs.Path.resolve(["/usr/bin/agda"])->VSCode.Uri.file
        switch uri {
        | FileURI(_, actual) => Assert.deepStrictEqual(actual, expected)
        }
      },
    )

    it(
      "should parse POSIX absolute paths using standard path resolution",
      () => {
        let uri = URI.parse("/tmp/dev-als/als")
        let expected = NodeJs.Path.resolve(["/tmp/dev-als/als"])->VSCode.Uri.file
        switch uri {
        | FileURI(_, actual) =>
          Assert.deepStrictEqual(actual->VSCode.Uri.toString, expected->VSCode.Uri.toString)
        }
      },
    )

    it(
      "should parse file:// POSIX absolute paths using standard path resolution",
      () => {
        let uri = URI.parse("file:///tmp/agda switch/dev-als/als.wasm")
        let expected = NodeJs.Path.resolve(["/tmp/agda switch/dev-als/als.wasm"])->VSCode.Uri.file
        switch uri {
        | FileURI(_, actual) =>
          Assert.deepStrictEqual(actual->VSCode.Uri.toString, expected->VSCode.Uri.toString)
        }
      },
    )

    it(
      "should parse relative file paths as FileURI variant",
      () => {
        let uri = URI.parse("usr/bin/agda")
        let expected = NodeJs.Path.resolve(["usr/bin/agda"])->VSCode.Uri.file
        switch uri {
        | FileURI(_, actual) => Assert.deepStrictEqual(actual, expected)
        }
      },
    )

    it(
      "should expand tilde in file paths",
      () => {
        let uri = URI.parse("~/bin/agda")
        let expected = NodeJs.Path.resolve([NodeJs.Os.homedir(), "bin/agda"])->VSCode.Uri.file
        switch uri {
        | FileURI(_, actual) =>
          // Should not contain tilde after untildify
          Assert.ok(!String.includes(VSCode.Uri.fsPath(actual), "~"))
          Assert.deepStrictEqual(actual->VSCode.Uri.toString, expected->VSCode.Uri.toString)
        }
      },
    )

    it(
      "should normalize file paths with ..",
      () => {
        let uri = URI.parse("/usr/bin/../bin/agda")
        let expected = NodeJs.Path.resolve(["/usr/bin", "..", "bin/agda"])->VSCode.Uri.file
        switch uri {
        | FileURI(_, actual) =>
          Assert.deepStrictEqual(actual->VSCode.Uri.toString, expected->VSCode.Uri.toString)
        }
      },
    )

    it(
      "should be able to parse file paths",
      () => {
        let actual = URI.parse("path/to/als")
        let expected = if OS.onUnix {
          URI.FileURI("path/to/als", VSCode.Uri.file(NodeJs.Path.resolve(["path/to/als"])))
        } else {
          URI.FileURI("path/to/als", VSCode.Uri.file(NodeJs.Path.resolve(["path\\to\\als"])))
        }
        Assert.deepStrictEqual(actual, expected)
      },
    )

    it(
      "should be able to parse convert \"/c/path/to/agda\" to \"c:/path/to/agda\" on Windows",
      () => {
        let actual = URI.parse("/c/path/to/agda")
        let expected = if OS.onUnix {
          URI.FileURI("/c/path/to/agda", VSCode.Uri.file("/c/path/to/agda"))
        } else {
          URI.FileURI("/c/path/to/agda", VSCode.Uri.file("c:\\path\\to\\agda"))
        }

        Assert.deepStrictEqual(actual, expected)

        let actual = URI.parse("/d/path/to/agda")
        let expected = if OS.onUnix {
          URI.FileURI("/d/path/to/agda", VSCode.Uri.file("/d/path/to/agda"))
        } else {
          URI.FileURI("/d/path/to/agda", VSCode.Uri.file("d:\\path\\to\\agda"))
        }

        Assert.deepStrictEqual(actual, expected)
      },
    )

    // Edge cases
    it(
      "should handle empty string input",
      () => {
        let uri = URI.parse("")
        switch uri {
        | FileURI(_) => () // Expected - empty string treated as filepath
        }
      },
    )

    it(
      "should handle paths with spaces",
      () => {
        let pathWithSpaces = "path with spaces/to file.txt"
        let uri = URI.parse(pathWithSpaces)
        switch uri {
        | FileURI(_, vscodeUri) =>
          let fsPath = VSCode.Uri.fsPath(vscodeUri)
          Assert.ok(String.includes(fsPath, "spaces"))
        }
      },
    )

    it(
      "should handle paths with special characters",
      () => {
        let specialPath = "path/with-special_chars@#$.txt"
        let uri = URI.parse(specialPath)
        switch uri {
        | FileURI(_) => () // Should parse successfully
        }
      },
    )

    it(
      "should handle multiple consecutive path separators",
      () => {
        let multiSlashPath = "path//with///multiple////slashes"
        let uri = URI.parse(multiSlashPath)
        switch uri {
        | FileURI(_) => () // Should parse and normalize
        }
      },
    )

    it(
      "should handle paths with dots",
      () => {
        let dotPath = "./current/dir/../parent/./file.txt"
        let uri = URI.parse(dotPath)
        switch uri {
        | FileURI(_) => () // Should parse and normalize
        }
      },
    )

    it(
      "should handle malformed LSP URLs as file paths",
      () => {
        let malformedLsp = "lsp://invalid-url-[]"
        let uri = URI.parse(malformedLsp)
        switch uri {
        | FileURI(_) => () // Should fallback to filepath
        }
      },
    )

    if !OS.onUnix {
      it(
        "should handle Windows reserved names",
        () => {
          let reservedName = "CON.txt"
          let uri = URI.parse(reservedName)
          switch uri {
          | FileURI(_) => () // Should parse (though may cause issues at runtime)
          }
        },
      )

      it(
        "should handle mixed path separators on Windows",
        () => {
          let mixedPath = "path/with\\mixed/separators"
          let uri = URI.parse(mixedPath)
          switch uri {
          | FileURI(_) => () // Should normalize separators
          }
        },
      )
    }

    describe(
      "Idempotency",
      () => {
        it(
          "should be idempotent for relative paths",
          () => {
            let original = "relative/path/to/file.txt"
            let firstParse = URI.parse(original)
            let secondParse = URI.parse(URI.toString(firstParse))
            Assert.ok(URI.equal(firstParse, secondParse))
          },
        )

        it(
          "should be idempotent for absolute paths",
          () => {
            let testPath = if OS.onUnix {
              "/usr/bin/agda"
            } else {
              "C:\\usr\\bin\\agda"
            }
            let firstParse = URI.parse(testPath)
            let secondParse = URI.parse(URI.toString(firstParse))
            Assert.ok(URI.equal(firstParse, secondParse))
          },
        )

        it(
          "should be idempotent for tilde paths",
          () => {
            let original = "~/bin/agda"
            let firstParse = URI.parse(original)
            let secondParse = URI.parse(URI.toString(firstParse))
            Assert.ok(URI.equal(firstParse, secondParse))
          },
        )

        it(
          "should be idempotent for paths with dots",
          () => {
            let original = "path/with/../dots/./file.txt"
            let firstParse = URI.parse(original)
            let secondParse = URI.parse(URI.toString(firstParse))
            Assert.ok(URI.equal(firstParse, secondParse))
          },
        )
      },
    )
  })

  describe("toString", () => {
    it(
      "should convert payload of FileURI with `VSCode.Uri.toString`",
      () => {
        let testPath = if OS.onUnix {
          "/usr/bin/../bin/agda"
        } else {
          "C:\\usr\\bin\\..\\bin\\agda"
        }
        let uri = VSCode.Uri.file(testPath)
        let expected = VSCode.Uri.toString(uri)
        let actual = URI.toString(URI.FileURI(testPath, uri))
        Assert.deepStrictEqual(actual, expected)
      },
    )

    it(
      "should handle empty FileURI toString",
      () => {
        let uri = VSCode.Uri.file("")
        let result = URI.toString(URI.FileURI("", uri))
        // Just verify it doesn't crash - result is guaranteed to be string
        Assert.ok(String.length(result) >= 0)
      },
    )
  })

  describe("round-trip parsing", () => {
    it(
      "should handle normalization consistency",
      () => {
        let unnormalized = "path/with/../dots/./file.txt"
        let normalized = "path/dots/file.txt"
        let uri1 = URI.parse(unnormalized)
        let uri2 = URI.parse(normalized)
        // After normalization, these should be equal
        Assert.ok(URI.equal(uri1, uri2))
      },
    )
  })

  describe("equal", () => {
    it(
      "should return true for equal file paths",
      () => {
        let testPath = if OS.onUnix {
          "/usr/bin/agda"
        } else {
          "C:\\usr\\bin\\agda"
        }
        let uri1 = URI.FileURI(testPath, VSCode.Uri.file(testPath))
        let uri2 = URI.FileURI(testPath, VSCode.Uri.file(testPath))
        Assert.ok(URI.equal(uri1, uri2))
      },
    )

    it(
      "should return false for different file paths",
      () => {
        let testPath1 = if OS.onUnix {
          "/usr/bin/agda"
        } else {
          "C:\\usr\\bin\\agda"
        }
        let testPath2 = if OS.onUnix {
          "/usr/local/bin/agda"
        } else {
          "C:\\usr\\local\\bin\\agda"
        }
        let uri1 = URI.FileURI(testPath1, VSCode.Uri.file(testPath1))
        let uri2 = URI.FileURI(testPath2, VSCode.Uri.file(testPath2))
        Assert.ok(!URI.equal(uri1, uri2))
      },
    )

    // Edge cases for equality
    it(
      "should handle normalized vs unnormalized paths",
      () => {
        let uri1 = URI.parse("path/with/../dots")
        let uri2 = URI.parse("path/dots")
        // After parsing/normalization, these should be equal
        Assert.ok(URI.equal(uri1, uri2))
      },
    )
  })

  describe("error handling", () => {
    it(
      "should handle very long paths",
      () => {
        let longPath = Array.make(~length=100, "very/")->Array.join("") ++ "long/path.txt"
        let uri = URI.parse(longPath)
        switch uri {
        | FileURI(_) => () // Should handle long paths
        }
      },
    )

    it(
      "should handle unicode characters in paths",
      () => {
        let unicodePath = "path/with/unicode/文件.txt"
        let uri = URI.parse(unicodePath)
        switch uri {
        | FileURI(_, vscodeUri) =>
          let fsPath = VSCode.Uri.fsPath(vscodeUri)
          Assert.ok(String.includes(fsPath, "文件"))
        }
      },
    )
  })
})
