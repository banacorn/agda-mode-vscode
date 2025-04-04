// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Assert = require("assert");
var Parser$AgdaModeVscode = require("../../../src/Parser/Parser.bs.js");
var Response$AgdaModeVscode = require("../../../src/Response.bs.js");
var Test__Util$AgdaModeVscode = require("../Test__Util.bs.js");
var Test__Parser__SExpression$AgdaModeVscode = require("./Test__Parser__SExpression.bs.js");

function toPrioritizedResponses(exprs) {
  return exprs.map(Response$AgdaModeVscode.Prioritized.parse).map(function (x) {
                if (x.TAG === "Ok") {
                  return [x._0];
                }
                Assert.fail(Parser$AgdaModeVscode.$$Error.toString(x._0));
                return [];
              }).flat();
}

describe("when parsing responses", (function () {
        Test__Util$AgdaModeVscode.Golden.getGoldenFilepathsSync("../../../../test/tests/Parser/Response").forEach(function (filepath) {
              it("should golden test " + filepath, (async function () {
                      var raw = await Test__Util$AgdaModeVscode.Golden.readFile(filepath);
                      return Test__Util$AgdaModeVscode.Golden.compare(Test__Util$AgdaModeVscode.Golden.map(Test__Util$AgdaModeVscode.Golden.map(Test__Util$AgdaModeVscode.Golden.map(raw, (function (extra) {
                                                return Test__Parser__SExpression$AgdaModeVscode.parseSExpression([], extra);
                                              })), toPrioritizedResponses), (function (extra) {
                                        return Test__Util$AgdaModeVscode.Strings.unlinesWith(Response$AgdaModeVscode.Prioritized.toString, extra);
                                      })));
                    }));
            });
      }));

exports.toPrioritizedResponses = toPrioritizedResponses;
/*  Not a pure module */
