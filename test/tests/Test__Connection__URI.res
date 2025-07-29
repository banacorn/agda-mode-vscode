open Mocha

module URI = Connection__URI

describe("Connection__URI", () => {
  describe("parse", () => {
    it(
      "should parse lsp:// URLs as LspURI variant",
      () => {
        let uri = URI.parse("lsp://localhost:8080")
        switch uri {
        | LspURI(url) => Assert.deepStrictEqual(url.protocol, "lsp:")
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
      "should parse regular file paths as FileURI variant",
      () => {
        let uri = URI.parse("/usr/bin/agda")
        switch uri {
        | FileURI(vscodeUri) => Assert.deepStrictEqual(VSCode.Uri.fsPath(vscodeUri), "/usr/bin/agda")
        | LspURI(_) => Assert.fail("Expected FileURI variant")
        }
      },
    )

    it(
      "should expand tilde in file paths",
      () => {
        let uri = URI.parse("~/bin/agda")
        switch uri {
        | FileURI(vscodeUri) =>
          // Should not contain tilde after untildify
          let path = VSCode.Uri.fsPath(vscodeUri)
          Assert.ok(!String.includes(path, "~"))
        | LspURI(_) => Assert.fail("Expected FileURI variant")
        }
      },
    )

    it(
      "should normalize file paths",
      () => {
        let uri = URI.parse("/usr/bin/../bin/agda")
        switch uri {
        | FileURI(vscodeUri) => Assert.deepStrictEqual(VSCode.Uri.fsPath(vscodeUri), "/usr/bin/agda")
        | LspURI(_) => Assert.fail("Expected FileURI variant")
        }
      },
    )


    it(
      "should be able to parse file paths",
      () => {
        let actual = URI.parse("path/to/als")
        let expected = if OS.onUnix {
          URI.FileURI(VSCode.Uri.file("path/to/als"))
        } else {
          URI.FileURI(VSCode.Uri.file("path\\to\\als"))
        }
        Assert.deepStrictEqual(actual, expected)
      },
    )

    it(
      "should be able to parse convert \"/c/path/to/agda\" to \"c:/path/to/agda\" on Windows",
      () => {
        let actual = URI.parse("/c/path/to/agda")
        let expected = if OS.onUnix {
          URI.FileURI(VSCode.Uri.file("/c/path/to/agda"))
        } else {
          URI.FileURI(VSCode.Uri.file("c:\\path\\to\\agda"))
        }

        Assert.deepStrictEqual(actual, expected)

        let actual = URI.parse("/d/path/to/agda")
        let expected = if OS.onUnix {
          URI.FileURI(VSCode.Uri.file("/d/path/to/agda"))
        } else {
          URI.FileURI(VSCode.Uri.file("d:\\path\\to\\agda"))
        }

        Assert.deepStrictEqual(actual, expected)
      },
    )

  })

  describe("toString", () => {
    it(
      "should convert FileURI to normalized string",
      () => {
        let vscodeUri = VSCode.Uri.file("/usr/bin/../bin/agda")
        let uri = URI.FileURI(vscodeUri)
        let result = URI.toString(uri)
        Assert.deepStrictEqual(result, "/usr/bin/agda")
      },
    )

    it(
      "should convert LspURI to string",
      () => {
        let url = NodeJs.Url.make("lsp://localhost:8080")
        let uri = URI.LspURI(url)
        let result = URI.toString(uri)
        Assert.deepStrictEqual(result, "lsp://localhost:8080")
      },
    )
  })

  describe("equal", () => {
    it(
      "should return true for equal file paths",
      () => {
        let uri1 = URI.FileURI(VSCode.Uri.file("/usr/bin/agda"))
        let uri2 = URI.FileURI(VSCode.Uri.file("/usr/bin/agda"))
        Assert.deepStrictEqual(URI.equal(uri1, uri2), true)
      },
    )

    it(
      "should return false for different file paths",
      () => {
        let uri1 = URI.FileURI(VSCode.Uri.file("/usr/bin/agda"))
        let uri2 = URI.FileURI(VSCode.Uri.file("/usr/local/bin/agda"))
        Assert.deepStrictEqual(URI.equal(uri1, uri2), false)
      },
    )

    it(
      "should return true for equal LSP URIs",
      () => {
        let url1 = NodeJs.Url.make("lsp://localhost:8080")
        let url2 = NodeJs.Url.make("lsp://localhost:8080")
        let uri1 = URI.LspURI(url1)
        let uri2 = URI.LspURI(url2)
        Assert.deepStrictEqual(URI.equal(uri1, uri2), true)
      },
    )

    it(
      "should return false for different LSP URIs",
      () => {
        let url1 = NodeJs.Url.make("lsp://localhost:8080")
        let url2 = NodeJs.Url.make("lsp://localhost:9090")
        let uri1 = URI.LspURI(url1)
        let uri2 = URI.LspURI(url2)
        Assert.deepStrictEqual(URI.equal(uri1, uri2), false)
      },
    )

    it(
      "should return false for mixed FileURI and LspURI",
      () => {
        let uri1 = URI.FileURI(VSCode.Uri.file("/usr/bin/agda"))
        let url = NodeJs.Url.make("lsp://localhost:8080")
        let uri2 = URI.LspURI(url)
        Assert.deepStrictEqual(URI.equal(uri1, uri2), false)
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
      "should preserve file paths through parse -> toString",
      () => {
        let original = "/usr/bin/agda"
        let parsed = URI.parse(original)
        let stringified = URI.toString(parsed)
        Assert.deepStrictEqual(stringified, original)
      },
    )
  })
})
