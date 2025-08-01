// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_exn = require("rescript/lib/js/js_exn.js");
var Caml_js_exceptions = require("rescript/lib/js/caml_js_exceptions.js");
var Item$AgdaModeVscode = require("../../../View/Component/Item.bs.js");
var Util$AgdaModeVscode = require("../../../Util/Util.bs.js");
var Json$JsonCombinators = require("@glennsl/rescript-json-combinators/lib/js/src/Json.bs.js");
var Token$AgdaModeVscode = require("../../../Tokens/Token.bs.js");
var Parser$AgdaModeVscode = require("../../../Parser/Parser.bs.js");
var Response$AgdaModeVscode = require("../../../Response.bs.js");
var Json_Decode$JsonCombinators = require("@glennsl/rescript-json-combinators/lib/js/src/Json_Decode.bs.js");
var Connection__Scheduler$AgdaModeVscode = require("../../Shared/Connection__Scheduler.bs.js");
var Connection__Endpoint__ALS__Error$AgdaModeVscode = require("./Connection__Endpoint__ALS__Error.bs.js");
var Connection__Endpoint__Protocol__LSP$AgdaModeVscode = require("../Protocol/Connection__Endpoint__Protocol__LSP.bs.js");

var encode = Util$AgdaModeVscode.Encode.sum(function (x) {
      if (typeof x !== "object") {
        return {
                TAG: "TagOnly",
                _0: "CmdReqSYN"
              };
      } else {
        return {
                TAG: "Payload",
                _0: "CmdReq",
                _1: x._0
              };
      }
    });

var CommandReq = {
  encode: encode
};

function fromJsError(error) {
  return (function (e) {return e.toString()})(error);
}

var decode = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "CmdRes" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.option(Connection__Endpoint__ALS__Error$AgdaModeVscode.CommandErr.decode), (function (error) {
                            return {
                                    TAG: "Result",
                                    _0: error
                                  };
                          }))
                  };
        case "CmdResACK" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (version) {
                            return {
                                    TAG: "ACK",
                                    _0: version
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[Connection.Target.ALS.CommandRes] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var CommandRes = {
  fromJsError: fromJsError,
  decode: decode
};

var decode$1 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "DisplayInfoAllGoalsWarnings" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Util$AgdaModeVscode.Decode.tuple5(Json_Decode$JsonCombinators.string, Json_Decode$JsonCombinators.array(Item$AgdaModeVscode.decode), Json_Decode$JsonCombinators.array(Item$AgdaModeVscode.decode), Json_Decode$JsonCombinators.array(Json_Decode$JsonCombinators.string), Json_Decode$JsonCombinators.array(Json_Decode$JsonCombinators.string)), (function (param) {
                            return {
                                    TAG: "AllGoalsWarnings",
                                    _0: param[0],
                                    _1: param[1],
                                    _2: param[2],
                                    _3: param[3],
                                    _4: param[4]
                                  };
                          }))
                  };
        case "DisplayInfoAuto" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (body) {
                            return {
                                    TAG: "Auto",
                                    _0: body
                                  };
                          }))
                  };
        case "DisplayInfoCompilationOk" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.pair(Json_Decode$JsonCombinators.array(Json_Decode$JsonCombinators.string), Json_Decode$JsonCombinators.array(Json_Decode$JsonCombinators.string)), (function (param) {
                            return {
                                    TAG: "CompilationOk",
                                    _0: param[0],
                                    _1: param[1]
                                  };
                          }))
                  };
        case "DisplayInfoCurrentGoal" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Item$AgdaModeVscode.decode, (function (body) {
                            return {
                                    TAG: "CurrentGoal",
                                    _0: body
                                  };
                          }))
                  };
        case "DisplayInfoError" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (body) {
                            return {
                                    TAG: "Error'",
                                    _0: body
                                  };
                          }))
                  };
        case "DisplayInfoGeneric" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.pair(Json_Decode$JsonCombinators.string, Json_Decode$JsonCombinators.array(Item$AgdaModeVscode.decode)), (function (param) {
                            return {
                                    TAG: "Generic",
                                    _0: param[0],
                                    _1: param[1]
                                  };
                          }))
                  };
        case "DisplayInfoInferredType" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Item$AgdaModeVscode.decode, (function (body) {
                            return {
                                    TAG: "InferredType",
                                    _0: body
                                  };
                          }))
                  };
        case "DisplayInfoNormalForm" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (body) {
                            return {
                                    TAG: "NormalForm",
                                    _0: body
                                  };
                          }))
                  };
        case "DisplayInfoTime" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (body) {
                            return {
                                    TAG: "Time",
                                    _0: body
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[ALS.DisplayInfo] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var DisplayInfo = {
  decode: decode$1
};

function toString(x) {
  if (typeof x !== "object") {
    return "========";
  }
  switch (x.TAG) {
    case "ResponseNonLast" :
        return Response$AgdaModeVscode.toString(x._0);
    case "ResponseLast" :
        return "[Last " + String(x._0) + "] " + Response$AgdaModeVscode.toString(x._1);
    case "ResponseParseError" :
        return Parser$AgdaModeVscode.$$Error.toString(x._0);
    
  }
}

var decode$2 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "ResponseClearHighlightingNotOnlyTokenBased" :
        case "ResponseClearHighlightingTokenBased" :
            return {
                    TAG: "TagOnly",
                    _0: {
                      TAG: "ResponseNonLast",
                      _0: "ClearHighlighting"
                    }
                  };
        case "ResponseClearRunningInfo" :
            return {
                    TAG: "TagOnly",
                    _0: {
                      TAG: "ResponseNonLast",
                      _0: "ClearRunningInfo"
                    }
                  };
        case "ResponseDisplayInfo" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(decode$1, (function (info) {
                            switch (info.TAG) {
                              case "Generic" :
                                  return {
                                          TAG: "ResponseNonLast",
                                          _0: {
                                            TAG: "DisplayInfo",
                                            _0: {
                                              TAG: "Generic",
                                              _0: info._0,
                                              _1: info._1
                                            }
                                          }
                                        };
                              case "CompilationOk" :
                                  return {
                                          TAG: "ResponseNonLast",
                                          _0: {
                                            TAG: "DisplayInfo",
                                            _0: {
                                              TAG: "CompilationOkALS",
                                              _0: info._0,
                                              _1: info._1
                                            }
                                          }
                                        };
                              case "AllGoalsWarnings" :
                                  return {
                                          TAG: "ResponseNonLast",
                                          _0: {
                                            TAG: "DisplayInfo",
                                            _0: {
                                              TAG: "AllGoalsWarningsALS",
                                              _0: info._0,
                                              _1: info._1,
                                              _2: info._2,
                                              _3: info._3,
                                              _4: info._4
                                            }
                                          }
                                        };
                              case "CurrentGoal" :
                                  return {
                                          TAG: "ResponseNonLast",
                                          _0: {
                                            TAG: "DisplayInfo",
                                            _0: {
                                              TAG: "CurrentGoalALS",
                                              _0: info._0
                                            }
                                          }
                                        };
                              case "InferredType" :
                                  return {
                                          TAG: "ResponseNonLast",
                                          _0: {
                                            TAG: "DisplayInfo",
                                            _0: {
                                              TAG: "InferredTypeALS",
                                              _0: info._0
                                            }
                                          }
                                        };
                              case "Auto" :
                                  return {
                                          TAG: "ResponseNonLast",
                                          _0: {
                                            TAG: "DisplayInfo",
                                            _0: {
                                              TAG: "Auto",
                                              _0: info._0
                                            }
                                          }
                                        };
                              case "Error'" :
                                  return {
                                          TAG: "ResponseNonLast",
                                          _0: {
                                            TAG: "DisplayInfo",
                                            _0: {
                                              TAG: "Error",
                                              _0: info._0
                                            }
                                          }
                                        };
                              case "Time" :
                                  return {
                                          TAG: "ResponseNonLast",
                                          _0: {
                                            TAG: "DisplayInfo",
                                            _0: {
                                              TAG: "Time",
                                              _0: info._0
                                            }
                                          }
                                        };
                              case "NormalForm" :
                                  return {
                                          TAG: "ResponseNonLast",
                                          _0: {
                                            TAG: "DisplayInfo",
                                            _0: {
                                              TAG: "NormalForm",
                                              _0: info._0
                                            }
                                          }
                                        };
                              
                            }
                          }))
                  };
        case "ResponseDoneAborting" :
            return {
                    TAG: "TagOnly",
                    _0: {
                      TAG: "ResponseNonLast",
                      _0: "DoneAborting"
                    }
                  };
        case "ResponseDoneExiting" :
            return {
                    TAG: "TagOnly",
                    _0: {
                      TAG: "ResponseNonLast",
                      _0: "DoneExiting"
                    }
                  };
        case "ResponseEnd" :
            return {
                    TAG: "TagOnly",
                    _0: "ResponseEnd"
                  };
        case "ResponseGiveAction" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.pair(Json_Decode$JsonCombinators.$$int, Response$AgdaModeVscode.GiveAction.decode), (function (param) {
                            return {
                                    TAG: "ResponseNonLast",
                                    _0: {
                                      TAG: "GiveAction",
                                      _0: param[0],
                                      _1: param[1]
                                    }
                                  };
                          }))
                  };
        case "ResponseHighlightingInfoDirect" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Token$AgdaModeVscode.decodeResponseHighlightingInfoDirect, (function (param) {
                            return {
                                    TAG: "ResponseNonLast",
                                    _0: {
                                      TAG: "HighlightingInfoDirect",
                                      _0: param[0],
                                      _1: param[1]
                                    }
                                  };
                          }))
                  };
        case "ResponseHighlightingInfoIndirect" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (filePath) {
                            return {
                                    TAG: "ResponseNonLast",
                                    _0: {
                                      TAG: "HighlightingInfoIndirectJSON",
                                      _0: filePath
                                    }
                                  };
                          }))
                  };
        case "ResponseInteractionPoints" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.array(Json_Decode$JsonCombinators.$$int), (function (ids) {
                            return {
                                    TAG: "ResponseLast",
                                    _0: 1,
                                    _1: {
                                      TAG: "InteractionPoints",
                                      _0: ids
                                    }
                                  };
                          }))
                  };
        case "ResponseJumpToError" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.pair(Json_Decode$JsonCombinators.string, Json_Decode$JsonCombinators.$$int), (function (param) {
                            return {
                                    TAG: "ResponseLast",
                                    _0: 3,
                                    _1: {
                                      TAG: "JumpToError",
                                      _0: param[0],
                                      _1: param[1]
                                    }
                                  };
                          }))
                  };
        case "ResponseMakeCaseExtendedLambda" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.array(Json_Decode$JsonCombinators.string), (function (payload) {
                            return {
                                    TAG: "ResponseLast",
                                    _0: 2,
                                    _1: {
                                      TAG: "MakeCase",
                                      _0: "ExtendedLambda",
                                      _1: payload
                                    }
                                  };
                          }))
                  };
        case "ResponseMakeCaseFunction" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.array(Json_Decode$JsonCombinators.string), (function (payload) {
                            return {
                                    TAG: "ResponseLast",
                                    _0: 2,
                                    _1: {
                                      TAG: "MakeCase",
                                      _0: "Function",
                                      _1: payload
                                    }
                                  };
                          }))
                  };
        case "ResponseRunningInfo" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.pair(Json_Decode$JsonCombinators.$$int, Json_Decode$JsonCombinators.string), (function (param) {
                            return {
                                    TAG: "ResponseNonLast",
                                    _0: {
                                      TAG: "RunningInfo",
                                      _0: param[0],
                                      _1: param[1]
                                    }
                                  };
                          }))
                  };
        case "ResponseSolveAll" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.array(Json_Decode$JsonCombinators.pair(Json_Decode$JsonCombinators.$$int, Json_Decode$JsonCombinators.string)), (function (payloads) {
                            return {
                                    TAG: "ResponseLast",
                                    _0: 2,
                                    _1: {
                                      TAG: "SolveAll",
                                      _0: payloads
                                    }
                                  };
                          }))
                  };
        case "ResponseStatus" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.pair(Json_Decode$JsonCombinators.bool, Json_Decode$JsonCombinators.bool), (function (param) {
                            return {
                                    TAG: "ResponseNonLast",
                                    _0: {
                                      TAG: "Status",
                                      _0: param[0],
                                      _1: param[1]
                                    }
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[ALS.Response] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var ALSResponse = {
  DisplayInfo: DisplayInfo,
  toString: toString,
  decode: decode$2
};

function decodeResponse(json) {
  var reaction = Json$JsonCombinators.decode(json, decode$2);
  if (reaction.TAG === "Ok") {
    return {
            TAG: "Ok",
            _0: reaction._0
          };
  } else {
    return {
            TAG: "Error",
            _0: {
              TAG: "CannotDecodeResponse",
              _0: reaction._0,
              _1: json
            }
          };
  }
}

async function sendRequestPrim(client, request) {
  var json;
  try {
    json = await Connection__Endpoint__Protocol__LSP$AgdaModeVscode.sendRequest(client, encode(request));
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Js_exn.$$Error) {
      return {
              TAG: "Error",
              _0: {
                TAG: "ConnectionError",
                _0: exn._1
              }
            };
    }
    throw exn;
  }
  if (json.TAG === "Ok") {
    var json$1 = json._0;
    var response = Json$JsonCombinators.decode(json$1, decode);
    if (response.TAG === "Ok") {
      return {
              TAG: "Ok",
              _0: response._0
            };
    } else {
      return {
              TAG: "Error",
              _0: {
                TAG: "CannotDecodeCommandRes",
                _0: response._0,
                _1: json$1
              }
            };
    }
  } else {
    return {
            TAG: "Error",
            _0: {
              TAG: "ConnectionError",
              _0: json._0
            }
          };
  }
}

async function make(method, lspOptions, options) {
  var error;
  try {
    error = await Connection__Endpoint__Protocol__LSP$AgdaModeVscode.make("agda", "Agda Language Server", method, lspOptions, options);
  }
  catch (raw_error){
    var error$1 = Caml_js_exceptions.internalToOCamlException(raw_error);
    if (error$1.RE_EXN_ID === Js_exn.$$Error) {
      return {
              TAG: "Error",
              _0: {
                TAG: "ConnectionError",
                _0: error$1._1
              }
            };
    }
    throw error$1;
  }
  if (error.TAG !== "Ok") {
    return {
            TAG: "Error",
            _0: {
              TAG: "ConnectionError",
              _0: error._0
            }
          };
  }
  var client = error._0;
  var error$2 = await sendRequestPrim(client, "SYN");
  if (error$2.TAG !== "Ok") {
    return {
            TAG: "Error",
            _0: error$2._0
          };
  }
  var agdaVersion = error$2._0;
  if (agdaVersion.TAG === "ACK") {
    return {
            TAG: "Ok",
            _0: {
              client: client,
              agdaVersion: agdaVersion._0,
              method: method
            }
          };
  } else {
    return {
            TAG: "Error",
            _0: "Initialize"
          };
  }
}

async function destroy(self) {
  var result;
  try {
    result = await Connection__Endpoint__Protocol__LSP$AgdaModeVscode.destroy(self.client);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Js_exn.$$Error) {
      return {
              TAG: "Error",
              _0: {
                TAG: "ConnectionError",
                _0: exn._1
              }
            };
    }
    throw exn;
  }
  if (result.TAG === "Ok") {
    return {
            TAG: "Ok",
            _0: result._0
          };
  } else {
    return {
            TAG: "Error",
            _0: {
              TAG: "ConnectionError",
              _0: result._0
            }
          };
  }
}

async function sendRequest(self, request, handler) {
  var handler$1 = function (response) {
    return handler(response);
  };
  var scheduler = Connection__Scheduler$AgdaModeVscode.make();
  var match = Util$AgdaModeVscode.Promise_.pending();
  var resolve = match[1];
  var stopListeningForNotifications = Connection__Endpoint__Protocol__LSP$AgdaModeVscode.onRequest(self.client, (async function (json) {
          var error = decodeResponse(json);
          if (error.TAG === "Ok") {
            var responses = error._0;
            if (typeof responses !== "object") {
              resolve({
                    TAG: "Ok",
                    _0: undefined
                  });
            } else {
              switch (responses.TAG) {
                case "ResponseNonLast" :
                    Connection__Scheduler$AgdaModeVscode.runNonLast(scheduler, handler$1, responses._0);
                    break;
                case "ResponseLast" :
                    Connection__Scheduler$AgdaModeVscode.addLast(scheduler, responses._0, responses._1);
                    break;
                case "ResponseParseError" :
                    resolve({
                          TAG: "Error",
                          _0: {
                            TAG: "ResponseParseError",
                            _0: responses._0
                          }
                        });
                    break;
                
              }
            }
          } else {
            resolve({
                  TAG: "Error",
                  _0: error._0
                });
          }
          return {
                  TAG: "Ok",
                  _0: null
                };
        }));
  var error = await sendRequestPrim(self.client, {
        TAG: "Command",
        _0: request
      });
  var result;
  if (error.TAG === "Ok") {
    var match$1 = error._0;
    if (match$1.TAG === "ACK") {
      result = {
        TAG: "Error",
        _0: "Initialize"
      };
    } else {
      var error$1 = match$1._0;
      result = error$1 !== undefined ? ({
            TAG: "Error",
            _0: {
              TAG: "SendCommand",
              _0: error$1
            }
          }) : await match[0];
    }
  } else {
    result = {
      TAG: "Error",
      _0: error._0
    };
  }
  stopListeningForNotifications.dispose();
  await Connection__Scheduler$AgdaModeVscode.runLast(scheduler, handler$1);
  return result;
}

function getIPCMethod(conn) {
  return conn.method;
}

var Module = {
  make: make,
  destroy: destroy,
  sendRequest: sendRequest,
  getIPCMethod: getIPCMethod
};

var $$Scheduler;

var $$Error;

var LSP;

exports.$$Scheduler = $$Scheduler;
exports.$$Error = $$Error;
exports.LSP = LSP;
exports.CommandReq = CommandReq;
exports.CommandRes = CommandRes;
exports.ALSResponse = ALSResponse;
exports.Module = Module;
exports.make = make;
exports.destroy = destroy;
exports.sendRequest = sendRequest;
exports.getIPCMethod = getIPCMethod;
/* encode Not a pure module */
