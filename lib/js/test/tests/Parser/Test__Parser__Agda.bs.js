// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Assert = require("assert");
var Agda$AgdaModeVscode = require("../../../src/Agda.bs.js");
var RichText$AgdaModeVscode = require("../../../src/View/Component/RichText.bs.js");

describe("when running Agda.Expr.parse", (function () {
        it("should parse a plain string", (function () {
                var expected = [{
                    TAG: "Plain",
                    _0: "ℕ"
                  }];
                var actual = Agda$AgdaModeVscode.Expr.parse("ℕ");
                Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
              }));
        it("should parse a question mark", (function () {
                var expected = [{
                    TAG: "QuestionMark",
                    _0: 3
                  }];
                var actual = Agda$AgdaModeVscode.Expr.parse("?3");
                Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
              }));
        it("should parse a underscore", (function () {
                var expected = [{
                    TAG: "Underscore",
                    _0: "_4hi"
                  }];
                var actual = Agda$AgdaModeVscode.Expr.parse(" _4hi ");
                Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
              }));
      }));

describe("when running Agda.OutputConstraint.parse", (function () {
        it("should parse OfType", (function () {
                var expected = {
                  TAG: "OfType",
                  _0: RichText$AgdaModeVscode.string("x"),
                  _1: RichText$AgdaModeVscode.string("ℕ")
                };
                var actual = Agda$AgdaModeVscode.OutputConstraint.parse("x : ℕ");
                Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
              }));
        it("should parse JustType", (function () {
                var expected = {
                  TAG: "JustType",
                  _0: RichText$AgdaModeVscode.string("ℕ")
                };
                var actual = Agda$AgdaModeVscode.OutputConstraint.parse("Type ℕ");
                Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
              }));
        it("should parse JustSort on Windows", (function () {
                var expected = {
                  TAG: "JustSort",
                  _0: RichText$AgdaModeVscode.string("ℕ\r\n  ℕ")
                };
                var actual = Agda$AgdaModeVscode.OutputConstraint.parse("Sort ℕ\r\n  ℕ");
                Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
              }));
        it("should parse JustSort on Unix", (function () {
                var expected = {
                  TAG: "JustSort",
                  _0: RichText$AgdaModeVscode.string("ℕ\n  ℕ")
                };
                var actual = Agda$AgdaModeVscode.OutputConstraint.parse("Sort ℕ\n  ℕ");
                Curry._3(Assert.deepStrictEqual, actual, expected, undefined);
              }));
      }));

/*  Not a pure module */
