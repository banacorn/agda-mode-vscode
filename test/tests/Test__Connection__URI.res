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
      "should parse absolute file paths as FileURI variant",
      () => {
        let uri = URI.parse("/usr/bin/agda")
        let expected = NodeJs.Path.resolve(["/usr/bin/agda"])->VSCode.Uri.file
        switch uri {
        | FileURI(actual) => Assert.deepStrictEqual(actual, expected)
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
        | FileURI(actual) => Assert.deepStrictEqual(actual, expected)
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
        | FileURI(actual) =>
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
        | FileURI(actual) =>
          // Assert.deepStrictEqual(VSCode.Uri.fsPath(vscodeUri), "/usr/bin/agda")
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
          URI.FileURI(VSCode.Uri.file(NodeJs.Path.resolve(["path/to/als"])))
        } else {
          URI.FileURI(VSCode.Uri.file(NodeJs.Path.resolve(["path\\to\\als"])))
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
      "should convert payload of FileURI with `VSCode.Uri.fsPath`",
      () => {
        let uri = VSCode.Uri.file("/usr/bin/../bin/agda")
        let expected = VSCode.Uri.fsPath(uri)
        let actual = URI.toString(URI.FileURI(uri))
        Assert.deepStrictEqual(actual, expected)
      },
    )

    it(
      "should convert payload of LspURI with `NodeJs.Url.toString`",
      () => {
        let url = NodeJs.Url.make("lsp://localhost:8080")
        let expected = url.toString()
        let actual = URI.toString(URI.LspURI(url))
        Assert.deepStrictEqual(actual, expected)
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
})