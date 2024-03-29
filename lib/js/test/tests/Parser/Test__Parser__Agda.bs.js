// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Mocha$BsMocha = require("bs-mocha/lib/js/src/Mocha.bs.js");
var Assert$BsMocha = require("bs-mocha/lib/js/src/Assert.bs.js");
var Agda$AgdaModeVscode = require("../../../src/Agda.bs.js");
var RichText$AgdaModeVscode = require("../../../src/View/Component/RichText.bs.js");

Mocha$BsMocha.describe("when running Agda.Expr.parse")(undefined, undefined, undefined, (function (param) {
        Mocha$BsMocha.it("should parse a plain string")(undefined, undefined, undefined, (function (param) {
                var expected = [{
                    TAG: 0,
                    _0: "ℕ",
                    [Symbol.for("name")]: "Plain"
                  }];
                var actual = Agda$AgdaModeVscode.Expr.parse("ℕ");
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse a question mark")(undefined, undefined, undefined, (function (param) {
                var expected = [{
                    TAG: 1,
                    _0: 3,
                    [Symbol.for("name")]: "QuestionMark"
                  }];
                var actual = Agda$AgdaModeVscode.Expr.parse("?3");
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse a underscore")(undefined, undefined, undefined, (function (param) {
                var expected = [{
                    TAG: 2,
                    _0: "_4hi",
                    [Symbol.for("name")]: "Underscore"
                  }];
                var actual = Agda$AgdaModeVscode.Expr.parse(" _4hi ");
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
      }));

Mocha$BsMocha.describe("when running Agda.OutputConstraint.parse")(undefined, undefined, undefined, (function (param) {
        Mocha$BsMocha.it("should parse OfType")(undefined, undefined, undefined, (function (param) {
                var expected = {
                  TAG: 0,
                  _0: RichText$AgdaModeVscode.string("x"),
                  _1: RichText$AgdaModeVscode.string("ℕ"),
                  [Symbol.for("name")]: "OfType"
                };
                var actual = Curry._1(Agda$AgdaModeVscode.OutputConstraint.parse, "x : ℕ");
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse JustType")(undefined, undefined, undefined, (function (param) {
                var expected = {
                  TAG: 1,
                  _0: RichText$AgdaModeVscode.string("ℕ"),
                  [Symbol.for("name")]: "JustType"
                };
                var actual = Curry._1(Agda$AgdaModeVscode.OutputConstraint.parse, "Type ℕ");
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse JustSort on Windows")(undefined, undefined, undefined, (function (param) {
                var expected = {
                  TAG: 2,
                  _0: RichText$AgdaModeVscode.string("ℕ\r\n  ℕ"),
                  [Symbol.for("name")]: "JustSort"
                };
                var actual = Curry._1(Agda$AgdaModeVscode.OutputConstraint.parse, "Sort ℕ\r\n  ℕ");
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse JustSort on Unix")(undefined, undefined, undefined, (function (param) {
                var expected = {
                  TAG: 2,
                  _0: RichText$AgdaModeVscode.string("ℕ\n  ℕ"),
                  [Symbol.for("name")]: "JustSort"
                };
                var actual = Curry._1(Agda$AgdaModeVscode.OutputConstraint.parse, "Sort ℕ\n  ℕ");
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
      }));

var Assert;

exports.Assert = Assert;
/*  Not a pure module */
