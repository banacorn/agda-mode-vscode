// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_dict = require("rescript/lib/js/js_dict.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Mocha$BsMocha = require("bs-mocha/lib/js/src/Mocha.bs.js");
var Assert$BsMocha = require("bs-mocha/lib/js/src/Assert.bs.js");
var Util$AgdaModeVscode = require("../../../src/Util/Util.bs.js");
var Emacs__Parser2$AgdaModeVscode = require("../../../src/View/Panel/Emacs/Emacs__Parser2.bs.js");

function tempNormalize(xs) {
  return Js_dict.map((function (value) {
                return Belt_Array.map(value, (function (x) {
                              return Util$AgdaModeVscode.$$String.unlines(Util$AgdaModeVscode.$$String.lines(x));
                            }));
              }), xs);
}

Mocha$BsMocha.describe("when running Emacs__Parser2.parseGoalType")(undefined, undefined, undefined, (function (param) {
        Mocha$BsMocha.it("should parse goal only")(undefined, undefined, undefined, (function (param) {
                var actual = Emacs__Parser2$AgdaModeVscode.parseGoalType("Goal: ℕ\n————————————————————————————————————————————————————————————");
                var expected = Js_dict.fromArray([[
                        "goal",
                        ["Goal: ℕ"]
                      ]]);
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse goal + have")(undefined, undefined, undefined, (function (param) {
                var actual = Emacs__Parser2$AgdaModeVscode.parseGoalType("Goal: ℕ\nHave: ℕ\n————————————————————————————————————————————————————————————");
                var expected = Js_dict.fromArray([
                      [
                        "goal",
                        ["Goal: ℕ"]
                      ],
                      [
                        "have",
                        ["Have: ℕ"]
                      ]
                    ]);
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse goal + have + context")(undefined, undefined, undefined, (function (param) {
                var actual = Emacs__Parser2$AgdaModeVscode.parseGoalType("Goal: ℕ\nHave: ℕ\n————————————————————————————————————————————————————————————\ny : ℕ\nx : ℕ");
                var expected = Js_dict.fromArray([
                      [
                        "goal",
                        ["Goal: ℕ"]
                      ],
                      [
                        "have",
                        ["Have: ℕ"]
                      ],
                      [
                        "interactionMetas",
                        [
                          "y : ℕ",
                          "x : ℕ"
                        ]
                      ]
                    ]);
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
      }));

Mocha$BsMocha.describe("when running Emacs__Parser2.parseAllGoalsWarnings")(undefined, undefined, undefined, (function (param) {
        Mocha$BsMocha.it("should parse goals only")(undefined, undefined, undefined, (function (param) {
                var actual = Emacs__Parser2$AgdaModeVscode.parseAllGoalsWarnings("*All Goals*", "\n?0 : ℕ\n?1 : ℕ\nSort _0  [ at /Users/banacorn/agda/examples/A.agda:11,5-20 ]\n");
                var expected = Js_dict.fromArray([
                      [
                        "interactionMetas",
                        [
                          "?0 : ℕ",
                          "?1 : ℕ"
                        ]
                      ],
                      [
                        "hiddenMetas",
                        ["Sort _0  [ at /Users/banacorn/agda/examples/A.agda:11,5-20 ]"]
                      ]
                    ]);
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse goals + errors")(undefined, undefined, undefined, (function (param) {
                var actual = Emacs__Parser2$AgdaModeVscode.parseAllGoalsWarnings("*All Goals, Errors*", "?0 : _2\n\n———— Errors ————————————————————————————————————————————————\nUnsolved constraints");
                var expected = Js_dict.fromArray([
                      [
                        "interactionMetas",
                        ["?0 : _2"]
                      ],
                      [
                        "errors",
                        ["Unsolved constraints"]
                      ]
                    ]);
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse goals that span multiple lines")(undefined, undefined, undefined, (function (param) {
                var actual = Emacs__Parser2$AgdaModeVscode.parseAllGoalsWarnings("*All Goals, Errors*", "?0\n  : BoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBool");
                var expected = tempNormalize(Js_dict.fromArray([[
                            "interactionMetas",
                            ["?0\n  : BoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBool"]
                          ]]));
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
      }));

Mocha$BsMocha.describe("when running Emacs__Parser2.parseError")(undefined, undefined, undefined, (function (param) {
        Mocha$BsMocha.it("should parse an error only")(undefined, undefined, undefined, (function (param) {
                var actual = Emacs__Parser2$AgdaModeVscode.parseError("/Users/banacorn/agda/examples/A.agda:15,1-2\nThe right-hand side can only be omitted if there is an absurd\npattern, () or {}, in the left-hand side.\nwhen checking that the clause a has type _8");
                var expected = tempNormalize(Js_dict.fromArray([[
                            "errors",
                            ["/Users/banacorn/agda/examples/A.agda:15,1-2\nThe right-hand side can only be omitted if there is an absurd\npattern, () or {}, in the left-hand side.\nwhen checking that the clause a has type _8"]
                          ]]));
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
        Mocha$BsMocha.it("should parse an error + warnings")(undefined, undefined, undefined, (function (param) {
                var actual = Emacs__Parser2$AgdaModeVscode.parseError("———— Error —————————————————————————————————————————————————\n/Users/banacorn/agda/examples/A.agda:15,1-2\nThe right-hand side can only be omitted if there is an absurd\npattern, () or {}, in the left-hand side.\nwhen checking that the clause a has type _8\n\n———— Warning(s) ————————————————————————————————————————————\n/Users/banacorn/agda/examples/A.agda:17,1-8\nThe following names are declared but not accompanied by a\ndefinition: boo\n/Users/banacorn/agda/examples/A.agda:9,1-10\nUnreachable clause\nwhen checking the definition of _+_");
                var expected = tempNormalize(Js_dict.fromArray([
                          [
                            "errors",
                            ["/Users/banacorn/agda/examples/A.agda:15,1-2\nThe right-hand side can only be omitted if there is an absurd\npattern, () or {}, in the left-hand side.\nwhen checking that the clause a has type _8"]
                          ],
                          [
                            "warnings",
                            [
                              "/Users/banacorn/agda/examples/A.agda:9,1-10\nUnreachable clause\nwhen checking the definition of _+_",
                              "/Users/banacorn/agda/examples/A.agda:17,1-8\nThe following names are declared but not accompanied by a\ndefinition: boo"
                            ]
                          ]
                        ]));
                Assert$BsMocha.deep_equal(undefined, actual, expected);
              }));
      }));

var Assert;

exports.Assert = Assert;
exports.tempNormalize = tempNormalize;
/*  Not a pure module */
