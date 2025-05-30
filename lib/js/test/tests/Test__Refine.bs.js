// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Assert = require("assert");
var Vscode = require("vscode");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Test__Util$AgdaModeVscode = require("./Test__Util.bs.js");

describe("agda-mode.refine", (function () {
        describe("Issue #158", (function () {
                var fileContent = {
                  contents: ""
                };
                before(async function () {
                      fileContent.contents = await Test__Util$AgdaModeVscode.$$File.read(Test__Util$AgdaModeVscode.Path.asset("Issue158.agda"));
                    });
                after(async function () {
                      return await Test__Util$AgdaModeVscode.$$File.write(Test__Util$AgdaModeVscode.Path.asset("Issue158.agda"), fileContent.contents);
                    });
                it("should result in the correct refinement", (async function () {
                        var ctx = await Test__Util$AgdaModeVscode.AgdaMode.makeAndLoad("Issue158.agda");
                        await Test__Util$AgdaModeVscode.AgdaMode.refine(ctx, Caml_option.some(new Vscode.Position(12, 9)), undefined);
                        var actual = await Test__Util$AgdaModeVscode.$$File.read(Test__Util$AgdaModeVscode.Path.asset("Issue158.agda"));
                        var expected = await Test__Util$AgdaModeVscode.$$File.read(Test__Util$AgdaModeVscode.Path.asset("Issue158.agda.out"));
                        return Curry._3(Assert.equal, actual, expected, undefined);
                      }));
              }));
      }));

/*  Not a pure module */
