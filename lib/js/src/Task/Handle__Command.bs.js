// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Task$AgdaModeVscode = require("./Task.bs.js");
var Caml_chrome_debugger = require("bs-platform/lib/js/caml_chrome_debugger.js");

function Impl(Editor) {
  var Task = Task$AgdaModeVscode.Impl(Editor);
  var handle = function (response) {
    if (typeof response === "number") {
      if (response !== 0) {
        return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                  /* Terminate */0,
                  /* [] */0
                ]);
      } else {
        return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                  /* SendRequest */Caml_chrome_debugger.variant("SendRequest", 0, [/* Load */0]),
                  /* [] */0
                ]);
      }
    } else {
      return /* :: */Caml_chrome_debugger.simpleVariant("::", [
                /* ViewRes */Caml_chrome_debugger.variant("ViewRes", 2, [response[0]]),
                /* [] */0
              ]);
    }
  };
  return {
          Task: Task,
          handle: handle
        };
}

exports.Impl = Impl;
/* Task-AgdaModeVscode Not a pure module */