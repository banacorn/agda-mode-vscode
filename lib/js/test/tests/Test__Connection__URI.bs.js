// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Assert = require("assert");
var Vscode = require("vscode");
var Nodeos = require("node:os");
var Nodeurl = require("node:url");
var Nodepath = require("node:path");
var Core__Array = require("@rescript/core/lib/js/src/Core__Array.bs.js");
var OS$AgdaModeVscode = require("../../src/Util/OS.bs.js");
var Connection__URI$AgdaModeVscode = require("../../src/Connection/Shared/Connection__URI.bs.js");

describe("Connection__URI", (function () {
        describe("parse", (function () {
                it("should parse lsp:// URLs as LspURI variant", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("lsp://localhost:8080");
                        if (uri.TAG !== "FileURI") {
                          return Curry._3(Assert.deepStrictEqual, uri._1.protocol, "lsp:", undefined);
                        }
                        Assert.fail("Expected LspURI variant");
                      }));
                it("should reject non-lsp URLs and treat as filepath", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("http://example.com");
                        if (uri.TAG === "FileURI") {
                          return ;
                        }
                        Assert.fail("Expected FileURI variant for non-lsp URL");
                      }));
                it("should parse absolute file paths as FileURI variant", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("/usr/bin/agda");
                        var expected = Vscode.Uri.file(Nodepath.resolve("/usr/bin/agda"));
                        if (uri.TAG === "FileURI") {
                          return Curry._3(Assert.deepStrictEqual, uri._1, expected, undefined);
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
                it("should parse relative file paths as FileURI variant", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("usr/bin/agda");
                        var expected = Vscode.Uri.file(Nodepath.resolve("usr/bin/agda"));
                        if (uri.TAG === "FileURI") {
                          return Curry._3(Assert.deepStrictEqual, uri._1, expected, undefined);
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
                it("should expand tilde in file paths", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("~/bin/agda");
                        var expected = Vscode.Uri.file(Nodepath.resolve(Nodeos.homedir(), "bin/agda"));
                        if (uri.TAG === "FileURI") {
                          var actual = uri._1;
                          Assert.ok(!actual.fsPath.includes("~"));
                          return Curry._3(Assert.deepStrictEqual, actual.toString(), expected.toString(), undefined);
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
                it("should normalize file paths with ..", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("/usr/bin/../bin/agda");
                        var expected = Vscode.Uri.file(Nodepath.resolve("/usr/bin", "..", "bin/agda"));
                        if (uri.TAG === "FileURI") {
                          return Curry._3(Assert.deepStrictEqual, uri._1.toString(), expected.toString(), undefined);
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
                it("should be able to parse file paths", (function () {
                        var actual = Connection__URI$AgdaModeVscode.parse("path/to/als");
                        var expected = OS$AgdaModeVscode.onUnix ? ({
                              TAG: "FileURI",
                              _0: "path/to/als",
                              _1: Vscode.Uri.file(Nodepath.resolve("path/to/als"))
                            }) : ({
                              TAG: "FileURI",
                              _0: "path/to/als",
                              _1: Vscode.Uri.file(Nodepath.resolve("path\\to\\als"))
                            });
                        Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
                      }));
                it("should be able to parse convert \"/c/path/to/agda\" to \"c:/path/to/agda\" on Windows", (function () {
                        var actual = Connection__URI$AgdaModeVscode.parse("/c/path/to/agda");
                        var expected = OS$AgdaModeVscode.onUnix ? ({
                              TAG: "FileURI",
                              _0: "/c/path/to/agda",
                              _1: Vscode.Uri.file("/c/path/to/agda")
                            }) : ({
                              TAG: "FileURI",
                              _0: "/c/path/to/agda",
                              _1: Vscode.Uri.file("c:\\path\\to\\agda")
                            });
                        Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
                        var actual$1 = Connection__URI$AgdaModeVscode.parse("/d/path/to/agda");
                        var expected$1 = OS$AgdaModeVscode.onUnix ? ({
                              TAG: "FileURI",
                              _0: "/d/path/to/agda",
                              _1: Vscode.Uri.file("/d/path/to/agda")
                            }) : ({
                              TAG: "FileURI",
                              _0: "/d/path/to/agda",
                              _1: Vscode.Uri.file("d:\\path\\to\\agda")
                            });
                        Curry._3(Assert.deepStrictEqual, actual$1, expected$1, undefined);
                      }));
                it("should handle empty string input", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("");
                        if (uri.TAG === "FileURI") {
                          return ;
                        }
                        Assert.fail("Expected FileURI variant for empty string");
                      }));
                it("should handle paths with spaces", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("path with spaces/to file.txt");
                        if (uri.TAG === "FileURI") {
                          var fsPath = uri._1.fsPath;
                          Assert.ok(fsPath.includes("spaces"));
                          return ;
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
                it("should handle paths with special characters", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("path/with-special_chars@#$.txt");
                        if (uri.TAG === "FileURI") {
                          return ;
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
                it("should handle multiple consecutive path separators", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("path//with///multiple////slashes");
                        if (uri.TAG === "FileURI") {
                          return ;
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
                it("should handle paths with dots", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("./current/dir/../parent/./file.txt");
                        if (uri.TAG === "FileURI") {
                          return ;
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
                it("should handle malformed LSP URLs as file paths", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("lsp://invalid-url-[]");
                        if (uri.TAG === "FileURI") {
                          return ;
                        }
                        Assert.fail("Expected FileURI variant for malformed LSP URL");
                      }));
                if (!OS$AgdaModeVscode.onUnix) {
                  it("should handle Windows reserved names", (function () {
                          var uri = Connection__URI$AgdaModeVscode.parse("CON.txt");
                          if (uri.TAG === "FileURI") {
                            return ;
                          }
                          Assert.fail("Expected FileURI variant");
                        }));
                  it("should handle mixed path separators on Windows", (function () {
                          var uri = Connection__URI$AgdaModeVscode.parse("path/with\\mixed/separators");
                          if (uri.TAG === "FileURI") {
                            return ;
                          }
                          Assert.fail("Expected FileURI variant");
                        }));
                }
                describe("Idempotency", (function () {
                        it("should be idempotent for relative paths", (function () {
                                var firstParse = Connection__URI$AgdaModeVscode.parse("relative/path/to/file.txt");
                                var secondParse = Connection__URI$AgdaModeVscode.parse(Connection__URI$AgdaModeVscode.toString(firstParse));
                                Assert.ok(Connection__URI$AgdaModeVscode.equal(firstParse, secondParse));
                              }));
                        it("should be idempotent for absolute paths", (function () {
                                var testPath = OS$AgdaModeVscode.onUnix ? "/usr/bin/agda" : "C:\\usr\\bin\\agda";
                                var firstParse = Connection__URI$AgdaModeVscode.parse(testPath);
                                var secondParse = Connection__URI$AgdaModeVscode.parse(Connection__URI$AgdaModeVscode.toString(firstParse));
                                Assert.ok(Connection__URI$AgdaModeVscode.equal(firstParse, secondParse));
                              }));
                        it("should be idempotent for tilde paths", (function () {
                                var firstParse = Connection__URI$AgdaModeVscode.parse("~/bin/agda");
                                var secondParse = Connection__URI$AgdaModeVscode.parse(Connection__URI$AgdaModeVscode.toString(firstParse));
                                Assert.ok(Connection__URI$AgdaModeVscode.equal(firstParse, secondParse));
                              }));
                        it("should be idempotent for paths with dots", (function () {
                                var firstParse = Connection__URI$AgdaModeVscode.parse("path/with/../dots/./file.txt");
                                var secondParse = Connection__URI$AgdaModeVscode.parse(Connection__URI$AgdaModeVscode.toString(firstParse));
                                Assert.ok(Connection__URI$AgdaModeVscode.equal(firstParse, secondParse));
                              }));
                        it("should be idempotent for LSP URLs", (function () {
                                var firstParse = Connection__URI$AgdaModeVscode.parse("lsp://localhost:8080");
                                var secondParse = Connection__URI$AgdaModeVscode.parse(Connection__URI$AgdaModeVscode.toString(firstParse));
                                Assert.ok(Connection__URI$AgdaModeVscode.equal(firstParse, secondParse));
                              }));
                      }));
              }));
        describe("toString", (function () {
                it("should convert payload of FileURI with `VSCode.Uri.toString`", (function () {
                        var testPath = OS$AgdaModeVscode.onUnix ? "/usr/bin/../bin/agda" : "C:\\usr\\bin\\..\\bin\\agda";
                        var uri = Vscode.Uri.file(testPath);
                        var expected = uri.toString();
                        var actual = Connection__URI$AgdaModeVscode.toString({
                              TAG: "FileURI",
                              _0: testPath,
                              _1: uri
                            });
                        Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
                      }));
                it("should handle empty FileURI toString", (function () {
                        var uri = Vscode.Uri.file("");
                        var result = Connection__URI$AgdaModeVscode.toString({
                              TAG: "FileURI",
                              _0: "",
                              _1: uri
                            });
                        Assert.ok(result.length >= 0);
                      }));
                it("should convert payload of LspURI with `NodeJs.Url.toString`", (function () {
                        var url = new Nodeurl.URL("lsp://localhost:8080");
                        var expected = url.toString();
                        var actual = Connection__URI$AgdaModeVscode.toString({
                              TAG: "LspURI",
                              _0: "lsp://localhost:8080",
                              _1: url
                            });
                        Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
                      }));
              }));
        describe("round-trip parsing", (function () {
                it("should preserve lsp:// URLs through parse -> toString", (function () {
                        var parsed = Connection__URI$AgdaModeVscode.parse("lsp://localhost:8080");
                        var stringified = Connection__URI$AgdaModeVscode.toString(parsed);
                        Assert.ok(stringified.startsWith("lsp://localhost:8080"));
                      }));
                it("should handle normalization consistency", (function () {
                        var uri1 = Connection__URI$AgdaModeVscode.parse("path/with/../dots/./file.txt");
                        var uri2 = Connection__URI$AgdaModeVscode.parse("path/dots/file.txt");
                        Assert.ok(Connection__URI$AgdaModeVscode.equal(uri1, uri2));
                      }));
              }));
        describe("equal", (function () {
                it("should return true for equal file paths", (function () {
                        var testPath = OS$AgdaModeVscode.onUnix ? "/usr/bin/agda" : "C:\\usr\\bin\\agda";
                        var uri1_1 = Vscode.Uri.file(testPath);
                        var uri1 = {
                          TAG: "FileURI",
                          _0: testPath,
                          _1: uri1_1
                        };
                        var uri2_1 = Vscode.Uri.file(testPath);
                        var uri2 = {
                          TAG: "FileURI",
                          _0: testPath,
                          _1: uri2_1
                        };
                        Assert.ok(Connection__URI$AgdaModeVscode.equal(uri1, uri2));
                      }));
                it("should return false for different file paths", (function () {
                        var testPath1 = OS$AgdaModeVscode.onUnix ? "/usr/bin/agda" : "C:\\usr\\bin\\agda";
                        var testPath2 = OS$AgdaModeVscode.onUnix ? "/usr/local/bin/agda" : "C:\\usr\\local\\bin\\agda";
                        var uri1_1 = Vscode.Uri.file(testPath1);
                        var uri1 = {
                          TAG: "FileURI",
                          _0: testPath1,
                          _1: uri1_1
                        };
                        var uri2_1 = Vscode.Uri.file(testPath2);
                        var uri2 = {
                          TAG: "FileURI",
                          _0: testPath2,
                          _1: uri2_1
                        };
                        Assert.ok(!Connection__URI$AgdaModeVscode.equal(uri1, uri2));
                      }));
                it("should return true for equal LSP URIs", (function () {
                        var url1 = new Nodeurl.URL("lsp://localhost:8080");
                        var url2 = new Nodeurl.URL("lsp://localhost:8080");
                        var uri1 = {
                          TAG: "LspURI",
                          _0: "lsp://localhost:8080",
                          _1: url1
                        };
                        var uri2 = {
                          TAG: "LspURI",
                          _0: "lsp://localhost:8080",
                          _1: url2
                        };
                        Assert.ok(Connection__URI$AgdaModeVscode.equal(uri1, uri2));
                      }));
                it("should return false for different LSP URIs", (function () {
                        var url1 = new Nodeurl.URL("lsp://localhost:8080");
                        var url2 = new Nodeurl.URL("lsp://localhost:9090");
                        var uri1 = {
                          TAG: "LspURI",
                          _0: "lsp://localhost:8080",
                          _1: url1
                        };
                        var uri2 = {
                          TAG: "LspURI",
                          _0: "lsp://localhost:9090",
                          _1: url2
                        };
                        Assert.ok(!Connection__URI$AgdaModeVscode.equal(uri1, uri2));
                      }));
                it("should return false for mixed FileURI and LspURI", (function () {
                        var testPath = OS$AgdaModeVscode.onUnix ? "/usr/bin/agda" : "C:\\usr\\bin\\agda";
                        var uri1_1 = Vscode.Uri.file(testPath);
                        var uri1 = {
                          TAG: "FileURI",
                          _0: testPath,
                          _1: uri1_1
                        };
                        var url = new Nodeurl.URL("lsp://localhost:8080");
                        var uri2 = {
                          TAG: "LspURI",
                          _0: "lsp://localhost:8080",
                          _1: url
                        };
                        Assert.ok(!Connection__URI$AgdaModeVscode.equal(uri1, uri2));
                      }));
                it("should handle normalized vs unnormalized paths", (function () {
                        var uri1 = Connection__URI$AgdaModeVscode.parse("path/with/../dots");
                        var uri2 = Connection__URI$AgdaModeVscode.parse("path/dots");
                        Assert.ok(Connection__URI$AgdaModeVscode.equal(uri1, uri2));
                      }));
              }));
        describe("error handling", (function () {
                it("should handle very long paths", (function () {
                        var longPath = Core__Array.make(100, "very/").join("") + "long/path.txt";
                        var uri = Connection__URI$AgdaModeVscode.parse(longPath);
                        if (uri.TAG === "FileURI") {
                          return ;
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
                it("should handle unicode characters in paths", (function () {
                        var uri = Connection__URI$AgdaModeVscode.parse("path/with/unicode/文件.txt");
                        if (uri.TAG === "FileURI") {
                          var fsPath = uri._1.fsPath;
                          Assert.ok(fsPath.includes("文件"));
                          return ;
                        }
                        Assert.fail("Expected FileURI variant");
                      }));
              }));
      }));

var URI;

exports.URI = URI;
/*  Not a pure module */
