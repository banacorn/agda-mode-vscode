open Mocha

module URI = Connection__URI

describe("Connection__URI", () => {
  // NOTE: DO NOT apply `VSCode.Uri.t`` directly on `Assert.deepStrictEqual`
  // because their values are computed lazily somehow, apply `VSCode.Uri.toString` before comparing!
  // To compare URIs, use `Assert.ok` with `Connection__URI.equal` instead.
  describe("parse", () => {
    it(
      "should parse lsp:// URLs as LspURI variant",
      () => {
        let uri = URI.parse("lsp://localhost:8080")
        switch uri {
        | LspURI(_, url) => Assert.deepStrictEqual(url.protocol, "lsp:")
        | FileURI(_) => Assert.fail("Expected LspURI variant")
        }
      },
    )

    it(
      "should reject non-lsp URLs and treat as filepath",
      () => {
        let uri = URI.parse("http://example.com")
        switch uri {
        | FileURI(_) => () // Expected
        | LspURI(_) => Assert.fail("Expected FileURI variant for non-lsp URL")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant for empty string")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant for malformed LSP URL")
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
          | LspURI(_) => Assert.fail("Expected FileURI variant")
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
          | LspURI(_) => Assert.fail("Expected FileURI variant")
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

        it(
          "should be idempotent for LSP URLs",
          () => {
            let original = "lsp://localhost:8080"
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

    it(
      "should convert payload of LspURI with `NodeJs.Url.toString`",
      () => {
        let url = NodeJs.Url.make("lsp://localhost:8080")
        let expected = url.toString()
        let actual = URI.toString(URI.LspURI("lsp://localhost:8080", url))
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })

  describe("round-trip parsing", () => {
    it(
      "should preserve lsp:// URLs through parse -> toString",
      () => {
        let original = "lsp://localhost:8080"
        let parsed = URI.parse(original)
        let stringified = URI.toString(parsed)
        // Note: URLs may have trailing slash added
        Assert.ok(String.startsWith(stringified, "lsp://localhost:8080"))
      },
    )

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

    it(
      "should return true for equal LSP URIs",
      () => {
        let url1 = NodeJs.Url.make("lsp://localhost:8080")
        let url2 = NodeJs.Url.make("lsp://localhost:8080")
        let uri1 = URI.LspURI("lsp://localhost:8080", url1)
        let uri2 = URI.LspURI("lsp://localhost:8080", url2)
        Assert.ok(URI.equal(uri1, uri2))
      },
    )

    it(
      "should return false for different LSP URIs",
      () => {
        let url1 = NodeJs.Url.make("lsp://localhost:8080")
        let url2 = NodeJs.Url.make("lsp://localhost:9090")
        let uri1 = URI.LspURI("lsp://localhost:8080", url1)
        let uri2 = URI.LspURI("lsp://localhost:9090", url2)
        Assert.ok(!URI.equal(uri1, uri2))
      },
    )

    it(
      "should return false for mixed FileURI and LspURI",
      () => {
        let testPath = if OS.onUnix {
          "/usr/bin/agda"
        } else {
          "C:\\usr\\bin\\agda"
        }
        let uri1 = URI.FileURI(testPath, VSCode.Uri.file(testPath))
        let url = NodeJs.Url.make("lsp://localhost:8080")
        let uri2 = URI.LspURI("lsp://localhost:8080", url)
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
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
        | LspURI(_) => Assert.fail("Expected FileURI variant")
        }
      },
    )
  })
})
