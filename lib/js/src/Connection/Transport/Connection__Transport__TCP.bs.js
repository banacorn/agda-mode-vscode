// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Nodenet = require("node:net");
var Util$AgdaModeVscode = require("../../Util/Util.bs.js");

function toString(x) {
  if (x.TAG === "Timeout") {
    return "Expected to connect within " + String(x._0) + "ms";
  } else {
    return Util$AgdaModeVscode.JsError.toString(x._0);
  }
}

var $$Error = {
  toString: toString
};

function probe(url, timeoutOpt) {
  var timeout = timeoutOpt !== undefined ? timeoutOpt : 1000;
  var connection = new Promise((function (resolve, param) {
          var socket = new Nodenet.Socket();
          socket.connect(url.port, url.hostname, (function () {
                        
                      })).once("connect", (function () {
                      socket.destroy(undefined);
                      resolve({
                            TAG: "Ok",
                            _0: undefined
                          });
                    })).once("error", (function (exn) {
                    resolve({
                          TAG: "Error",
                          _0: {
                            TAG: "OnError",
                            _0: exn
                          }
                        });
                  })).once("timeout", (function () {
                  resolve({
                        TAG: "Error",
                        _0: {
                          TAG: "Timeout",
                          _0: timeout
                        }
                      });
                }));
        }));
  var timeout$1 = async function () {
    await Util$AgdaModeVscode.Promise_.$$setTimeout(timeout);
    return {
            TAG: "Error",
            _0: {
              TAG: "Timeout",
              _0: timeout
            }
          };
  };
  return Promise.race([
              connection,
              timeout$1()
            ]);
}

exports.$$Error = $$Error;
exports.probe = probe;
/* node:net Not a pure module */
