// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Assert = require("assert");
var Test__Util$AgdaModeVscode = require("./Test__Util.bs.js");
var State__Connection$AgdaModeVscode = require("../../src/State/State__Connection.bs.js");

function run(normalization) {
  var filename = "ComputeNormalForm.agda";
  var fileContent = {
    contents: ""
  };
  beforeEach(async function () {
        fileContent.contents = await Test__Util$AgdaModeVscode.$$File.read(Test__Util$AgdaModeVscode.Path.asset(filename));
      });
  afterEach(async function () {
        return await Test__Util$AgdaModeVscode.$$File.write(Test__Util$AgdaModeVscode.Path.asset(filename), fileContent.contents);
      });
  it("should be responded with correct responses (goal)", (async function () {
          var ctx = await Test__Util$AgdaModeVscode.AgdaMode.makeAndLoad(filename);
          var responses = await State__Connection$AgdaModeVscode.sendRequestAndCollectResponses(ctx.state, {
                TAG: "ComputeNormalForm",
                _0: normalization,
                _1: "Z + S Z",
                _2: {
                  index: 0,
                  indexString: "0",
                  start: 137,
                  end: 144
                }
              });
          var filteredResponses = responses.filter(Test__Util$AgdaModeVscode.filteredResponse);
          return Curry._3(Assert.deepStrictEqual, filteredResponses, [{
                        TAG: "DisplayInfo",
                        _0: {
                          TAG: "NormalForm",
                          _0: "S Z"
                        }
                      }], undefined);
        }));
  it("should be responded with correct responses (global)", (async function () {
          var ctx = await Test__Util$AgdaModeVscode.AgdaMode.makeAndLoad(filename);
          var responses = await State__Connection$AgdaModeVscode.sendRequestAndCollectResponses(ctx.state, {
                TAG: "ComputeNormalFormGlobal",
                _0: normalization,
                _1: "Z + S Z"
              });
          var filteredResponses = responses.filter(Test__Util$AgdaModeVscode.filteredResponse);
          return Curry._3(Assert.deepStrictEqual, filteredResponses, [{
                        TAG: "DisplayInfo",
                        _0: {
                          TAG: "NormalForm",
                          _0: "S Z"
                        }
                      }], undefined);
        }));
}

describe("agda-mode.compute-normal-form", (function () {
        describe("DefaultCompute", (function () {
                run("DefaultCompute");
              }));
        describe("IgnoreAbstract", (function () {
                run("IgnoreAbstract");
              }));
      }));

exports.run = run;
/*  Not a pure module */
