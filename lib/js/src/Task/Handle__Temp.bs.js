// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var $$Promise = require("reason-promise/lib/js/src/js/promise.bs.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_primitive = require("bs-platform/lib/js/caml_primitive.js");
var Chan$AgdaModeVscode = require("../Util/Chan.bs.js");
var Task$AgdaModeVscode = require("./Task.bs.js");
var Util$AgdaModeVscode = require("../Util/Util.bs.js");
var State$AgdaModeVscode = require("../State.bs.js");
var Config$AgdaModeVscode = require("../Config.bs.js");
var Parser$AgdaModeVscode = require("../Parser.bs.js");
var Request$AgdaModeVscode = require("../Request.bs.js");
var Connection$AgdaModeVscode = require("../Connection.bs.js");
var Handle__Response$AgdaModeVscode = require("./Handle__Response.bs.js");
var Handle__Decoration$AgdaModeVscode = require("./Handle__Decoration.bs.js");

var tally = {
  contents: 0
};

var allDone = Chan$AgdaModeVscode.make(undefined);

function runNonLast(promise) {
  tally.contents = tally.contents + 1 | 0;
  return $$Promise.tap(promise, (function (param) {
                tally.contents = tally.contents - 1 | 0;
                if (tally.contents === 0) {
                  return Chan$AgdaModeVscode.emit(allDone, undefined);
                }
                
              }));
}

function onceDone(param) {
  if (tally.contents === 0) {
    return $$Promise.resolved(undefined);
  } else {
    return Chan$AgdaModeVscode.once(allDone);
  }
}

var Lock = {
  runNonLast: runNonLast,
  onceDone: onceDone
};

function sendAgdaRequest(dispatchCommand, state, request) {
  var displayConnectionError = function (error) {
    var match = Connection$AgdaModeVscode.$$Error.toString(error);
    return Task$AgdaModeVscode.display(state, {
                TAG: 3,
                _0: "Connection Error: " + match[0],
                [Symbol.for("name")]: "Error"
              }, {
                TAG: 0,
                _0: match[1],
                [Symbol.for("name")]: "Plain"
              });
  };
  var deferredLastResponses = [];
  var match = $$Promise.pending(undefined);
  var stopListener = match[1];
  var promise = match[0];
  var handle = {
    contents: undefined
  };
  var agdaResponseListener = function (x) {
    if (x.TAG) {
      displayConnectionError(x._0);
      return ;
    }
    var match = x._0;
    if (match) {
      var error = match._0;
      if (error.TAG) {
        var body = Parser$AgdaModeVscode.$$Error.toString(error._0);
        Task$AgdaModeVscode.display(state, {
              TAG: 3,
              _0: "Internal Parse Error",
              [Symbol.for("name")]: "Error"
            }, {
              TAG: 0,
              _0: body,
              [Symbol.for("name")]: "Plain"
            });
        return ;
      }
      var response = error._0;
      if (response.TAG) {
        deferredLastResponses.push([
              response._0,
              response._1
            ]);
        return ;
      }
      runNonLast(Handle__Response$AgdaModeVscode.handle(state, dispatchCommand, (function (param) {
                  return sendAgdaRequest(dispatchCommand, state, param);
                }), response._0));
      return ;
    }
    var deferredLastResponses$1 = Belt_Array.map(deferredLastResponses.sort(function (x, y) {
              return Caml_primitive.caml_int_compare(x[0], y[0]);
            }), (function (prim) {
            return prim[1];
          }));
    $$Promise.flatMap($$Promise.map($$Promise.flatMap($$Promise.tap(onceDone(undefined), (function (param) {
                        return Curry._1(stopListener, undefined);
                      })), (function (param) {
                    return Handle__Decoration$AgdaModeVscode.apply(state);
                  })), (function (param) {
                return Belt_Array.map(deferredLastResponses$1, (function (param) {
                              return Handle__Response$AgdaModeVscode.handle(state, dispatchCommand, (function (param) {
                                            return sendAgdaRequest(dispatchCommand, state, param);
                                          }), param);
                            }));
              })), Util$AgdaModeVscode.oneByOne);
    
  };
  return $$Promise.tap($$Promise.flatMap($$Promise.mapOk(State$AgdaModeVscode.connect(state), (function (connection) {
                        var $$document = state.editor.document;
                        var version = connection.metadata.version;
                        var filepath = Parser$AgdaModeVscode.filepath($$document.fileName);
                        var libraryPath = Config$AgdaModeVscode.getLibraryPath(undefined);
                        var highlightingMethod = Config$AgdaModeVscode.getHighlightingMethod(undefined);
                        var backend = Config$AgdaModeVscode.getBackend(undefined);
                        var encoded = Request$AgdaModeVscode.encode($$document, version, filepath, backend, libraryPath, highlightingMethod, request);
                        Connection$AgdaModeVscode.send(encoded, connection);
                        return connection;
                      })), (function (x) {
                    if (x.TAG) {
                      return $$Promise.flatMap(displayConnectionError(x._0), (function (param) {
                                    return promise;
                                  }));
                    }
                    handle.contents = Chan$AgdaModeVscode.on(x._0.chan, agdaResponseListener);
                    return promise;
                  })), (function (param) {
                return Belt_Option.forEach(handle.contents, (function (destroyListener) {
                              return Curry._1(destroyListener, undefined);
                            }));
              }));
}

exports.Lock = Lock;
exports.sendAgdaRequest = sendAgdaRequest;
/* allDone Not a pure module */