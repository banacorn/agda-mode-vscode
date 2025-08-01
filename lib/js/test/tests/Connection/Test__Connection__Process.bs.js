// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_exn = require("rescript/lib/js/js_exn.js");
var Util$AgdaModeVscode = require("../../../src/Util/Util.bs.js");
var Connection__Transport__Process$AgdaModeVscode = require("../../../src/Connection/Transport/Connection__Transport__Process.bs.js");

describe("Process Interface", (function () {
        describe("Use `echo` as the testing subject", (function () {
                it.skip("should trigger `close`", (async function () {
                        var $$process = Connection__Transport__Process$AgdaModeVscode.make("echo", ["hello"]);
                        var match = Util$AgdaModeVscode.Promise_.pending();
                        var reject = match[2];
                        var resolve = match[1];
                        var destructor = Connection__Transport__Process$AgdaModeVscode.onOutput($$process, (function (output) {
                                switch (output.TAG) {
                                  case "Stdout" :
                                      var output$1 = output._0;
                                      switch (output$1) {
                                        case "hello\n" :
                                        case "hello\r\n" :
                                            return ;
                                        default:
                                          return reject(Js_exn.raiseError("wrong output: " + output$1));
                                      }
                                  case "Stderr" :
                                      return resolve(Js_exn.raiseError("Stderr: " + output._0));
                                  case "Event" :
                                      var $$event = output._0;
                                      if (typeof $$event === "object" && $$event.TAG !== "OnError" && $$event._0 === 0) {
                                        return resolve();
                                      }
                                      return resolve(Js_exn.raiseError("Event: " + Connection__Transport__Process$AgdaModeVscode.$$Event.toString($$event)));
                                  
                                }
                              }));
                        await match[0];
                        return destructor();
                      }));
              }));
        describe("Use a non-existing command as the testing subject", (function () {
                it("should trigger receive something from stderr", (async function () {
                        var $$process = Connection__Transport__Process$AgdaModeVscode.make("echooo", ["hello"]);
                        var match = Util$AgdaModeVscode.Promise_.pending();
                        var reject = match[2];
                        var resolve = match[1];
                        var destructor = Connection__Transport__Process$AgdaModeVscode.onOutput($$process, (function (output) {
                                switch (output.TAG) {
                                  case "Stdout" :
                                      return reject(Js_exn.raiseError("wrong output: " + output._0));
                                  case "Stderr" :
                                      return resolve();
                                  case "Event" :
                                      return reject(Js_exn.raiseError("Event: " + Connection__Transport__Process$AgdaModeVscode.$$Event.toString(output._0)));
                                  
                                }
                              }));
                        await match[0];
                        return destructor();
                      }));
              }));
      }));

var Process;

exports.Process = Process;
/*  Not a pure module */
