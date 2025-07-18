// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var React = require("react");
var Chan$AgdaModeVscode = require("../Util/Chan.bs.js");
var Item$AgdaModeVscode = require("./Component/Item.bs.js");
var Link$AgdaModeVscode = require("./Component/Link.bs.js");
var Util$AgdaModeVscode = require("../Util/Util.bs.js");
var Common$AgdaModeVscode = require("./Common.bs.js");
var Translator$AgdaModeVscode = require("../InputMethod/Translator.bs.js");
var Json_Decode$JsonCombinators = require("@glennsl/rescript-json-combinators/lib/js/src/Json_Decode.bs.js");
var Json_Encode$JsonCombinators = require("@glennsl/rescript-json-combinators/lib/js/src/Json_Encode.bs.js");

function toString(x) {
  return x._0;
}

var decode = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "Error" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: "Error",
                                    _0: text
                                  };
                          }))
                  };
        case "Plain" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: "Plain",
                                    _0: text
                                  };
                          }))
                  };
        case "Success" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: "Success",
                                    _0: text
                                  };
                          }))
                  };
        case "Warning" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: "Warning",
                                    _0: text
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[Header] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var encode = Util$AgdaModeVscode.Encode.sum(function (x) {
      switch (x.TAG) {
        case "Plain" :
            return {
                    TAG: "Payload",
                    _0: "Plain",
                    _1: x._0
                  };
        case "Success" :
            return {
                    TAG: "Payload",
                    _0: "Success",
                    _1: x._0
                  };
        case "Warning" :
            return {
                    TAG: "Payload",
                    _0: "Warning",
                    _1: x._0
                  };
        case "Error" :
            return {
                    TAG: "Payload",
                    _0: "Error",
                    _1: x._0
                  };
        
      }
    });

var Header = {
  toString: toString,
  decode: decode,
  encode: encode
};

var decode$1 = Json_Decode$JsonCombinators.object(function (field) {
      return {
              body: field.required("body", Json_Decode$JsonCombinators.option(Json_Decode$JsonCombinators.string)),
              placeholder: field.required("placeholder", Json_Decode$JsonCombinators.option(Json_Decode$JsonCombinators.string)),
              value: field.required("value", Json_Decode$JsonCombinators.option(Json_Decode$JsonCombinators.string))
            };
    });

function encode$1(param) {
  return {
          body: Json_Encode$JsonCombinators.option(function (prim) {
                  return prim;
                })(param.body),
          placeholder: Json_Encode$JsonCombinators.option(function (prim) {
                  return prim;
                })(param.placeholder),
          value: Json_Encode$JsonCombinators.option(function (prim) {
                  return prim;
                })(param.value)
        };
}

var Prompt = {
  decode: decode$1,
  encode: encode$1
};

var Body = {};

var decode$2 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "Activate" :
            return {
                    TAG: "TagOnly",
                    _0: "Activate"
                  };
        case "BrowseDown" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseDown"
                  };
        case "BrowseLeft" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseLeft"
                  };
        case "BrowseRight" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseRight"
                  };
        case "BrowseUp" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseUp"
                  };
        case "Deactivate" :
            return {
                    TAG: "TagOnly",
                    _0: "Deactivate"
                  };
        case "Update" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.tuple3(Json_Decode$JsonCombinators.string, Translator$AgdaModeVscode.decode, Json_Decode$JsonCombinators.$$int), (function (param) {
                            return {
                                    TAG: "Update",
                                    _0: param[0],
                                    _1: param[1],
                                    _2: param[2]
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventToView.InputMethod] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var encode$2 = Util$AgdaModeVscode.Encode.sum(function (x) {
      if (typeof x === "object") {
        return {
                TAG: "Payload",
                _0: "Update",
                _1: Json_Encode$JsonCombinators.tuple3((function (prim) {
                          return prim;
                        }), Translator$AgdaModeVscode.encode, (function (prim) {
                          return prim;
                        }))([
                      x._0,
                      x._1,
                      x._2
                    ])
              };
      }
      switch (x) {
        case "Activate" :
            return {
                    TAG: "TagOnly",
                    _0: "Activate"
                  };
        case "Deactivate" :
            return {
                    TAG: "TagOnly",
                    _0: "Deactivate"
                  };
        case "BrowseUp" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseUp"
                  };
        case "BrowseRight" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseRight"
                  };
        case "BrowseDown" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseDown"
                  };
        case "BrowseLeft" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseLeft"
                  };
        
      }
    });

var InputMethod = {
  decode: decode$2,
  encode: encode$2
};

function toString$1(x) {
  if (typeof x !== "object") {
    return "PromptInterrupt";
  }
  switch (x.TAG) {
    case "Display" :
        return "Display " + x._0._0;
    case "Append" :
        return "Append " + x._0._0;
    case "SetStatus" :
        return "SetStatus " + x._0;
    case "PromptIMUpdate" :
        return "PromptIMUpdate " + x._0;
    case "InputMethod" :
        return "InputMethod";
    case "ConfigurationChange" :
        return "ConfigurationChange";
    
  }
}

var decode$3 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "Append" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.tuple2(decode, Json_Decode$JsonCombinators.array(Item$AgdaModeVscode.decode)), (function (param) {
                            return {
                                    TAG: "Append",
                                    _0: param[0],
                                    _1: param[1]
                                  };
                          }))
                  };
        case "ConfigurationChange" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: "ConfigurationChange",
                                    _0: text
                                  };
                          }))
                  };
        case "Display" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.tuple2(decode, Json_Decode$JsonCombinators.array(Item$AgdaModeVscode.decode)), (function (param) {
                            return {
                                    TAG: "Display",
                                    _0: param[0],
                                    _1: param[1]
                                  };
                          }))
                  };
        case "InputMethod" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(decode$2, (function (payload) {
                            return {
                                    TAG: "InputMethod",
                                    _0: payload
                                  };
                          }))
                  };
        case "PromptIMUpdate" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: "PromptIMUpdate",
                                    _0: text
                                  };
                          }))
                  };
        case "PromptInterrupt" :
            return {
                    TAG: "TagOnly",
                    _0: "PromptInterrupt"
                  };
        case "SetStatus" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: "SetStatus",
                                    _0: text
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventToView] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var encode$3 = Util$AgdaModeVscode.Encode.sum(function (x) {
      if (typeof x !== "object") {
        return {
                TAG: "TagOnly",
                _0: "PromptInterrupt"
              };
      }
      switch (x.TAG) {
        case "Display" :
            return {
                    TAG: "Payload",
                    _0: "Display",
                    _1: Json_Encode$JsonCombinators.tuple2(encode, Json_Encode$JsonCombinators.array(Item$AgdaModeVscode.encode))([
                          x._0,
                          x._1
                        ])
                  };
        case "Append" :
            return {
                    TAG: "Payload",
                    _0: "Append",
                    _1: Json_Encode$JsonCombinators.tuple2(encode, Json_Encode$JsonCombinators.array(Item$AgdaModeVscode.encode))([
                          x._0,
                          x._1
                        ])
                  };
        case "SetStatus" :
            return {
                    TAG: "Payload",
                    _0: "SetStatus",
                    _1: x._0
                  };
        case "PromptIMUpdate" :
            return {
                    TAG: "Payload",
                    _0: "PromptIMUpdate",
                    _1: x._0
                  };
        case "InputMethod" :
            return {
                    TAG: "Payload",
                    _0: "InputMethod",
                    _1: encode$2(x._0)
                  };
        case "ConfigurationChange" :
            return {
                    TAG: "Payload",
                    _0: "ConfigurationChange",
                    _1: x._0
                  };
        
      }
    });

var EventToView = {
  InputMethod: InputMethod,
  toString: toString$1,
  decode: decode$3,
  encode: encode$3
};

function toString$2(x) {
  return "Prompt " + x._0._0;
}

var decode$4 = Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.tuple2(decode, decode$1), (function (param) {
        return {
                TAG: "Prompt",
                _0: param[0],
                _1: param[1]
              };
      }));

function encode$4(request) {
  return Json_Encode$JsonCombinators.tuple2(encode, encode$1)([
              request._0,
              request._1
            ]);
}

var $$Request = {
  toString: toString$2,
  decode: decode$4,
  encode: encode$4
};

function toString$3(x) {
  if (x.TAG === "Request") {
    return toString$2(x._0);
  } else {
    return toString$1(x._0);
  }
}

var decode$5 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "Event" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(decode$3, (function (payload) {
                            return {
                                    TAG: "Event",
                                    _0: payload
                                  };
                          }))
                  };
        case "Request" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(decode$4, (function (payload) {
                            return {
                                    TAG: "Request",
                                    _0: payload
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[RequestOrEventToView] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var encode$5 = Util$AgdaModeVscode.Encode.sum(function (x) {
      if (x.TAG === "Request") {
        return {
                TAG: "Payload",
                _0: "Request",
                _1: encode$4(x._0)
              };
      } else {
        return {
                TAG: "Payload",
                _0: "Event",
                _1: encode$3(x._0)
              };
      }
    });

var RequestOrEventToView = {
  toString: toString$3,
  decode: decode$5,
  encode: encode$5
};

var decode$6 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "PromptInterrupted" :
            return {
                    TAG: "TagOnly",
                    _0: "PromptInterrupted"
                  };
        case "PromptSuccess" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (result) {
                            return {
                                    TAG: "PromptSuccess",
                                    _0: result
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[Response] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var encode$6 = Util$AgdaModeVscode.Encode.sum(function (x) {
      if (typeof x !== "object") {
        return {
                TAG: "TagOnly",
                _0: "PromptInterrupted"
              };
      } else {
        return {
                TAG: "Payload",
                _0: "PromptSuccess",
                _1: x._0
              };
      }
    });

var $$Response = {
  decode: decode$6,
  encode: encode$6
};

var decode$7 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "ChooseSymbol" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (symbol) {
                            return {
                                    TAG: "ChooseSymbol",
                                    _0: symbol
                                  };
                          }))
                  };
        case "InsertChar" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function ($$char) {
                            return {
                                    TAG: "InsertChar",
                                    _0: $$char
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventFromView.InputMethod] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var encode$7 = Util$AgdaModeVscode.Encode.sum(function (x) {
      if (x.TAG === "InsertChar") {
        return {
                TAG: "Payload",
                _0: "InsertChar",
                _1: x._0
              };
      } else {
        return {
                TAG: "Payload",
                _0: "ChooseSymbol",
                _1: x._0
              };
      }
    });

var InputMethod$1 = {
  decode: decode$7,
  encode: encode$7
};

var decode$8 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "BrowseDown" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseDown"
                  };
        case "BrowseLeft" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseLeft"
                  };
        case "BrowseRight" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseRight"
                  };
        case "BrowseUp" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseUp"
                  };
        case "Escape" :
            return {
                    TAG: "TagOnly",
                    _0: "Escape"
                  };
        case "KeyUpdate" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function ($$char) {
                            return {
                                    TAG: "KeyUpdate",
                                    _0: $$char
                                  };
                          }))
                  };
        case "MouseSelect" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Common$AgdaModeVscode.Interval.decode, (function (interval) {
                            return {
                                    TAG: "MouseSelect",
                                    _0: interval
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventFromView.PromptIMUpdate] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var encode$8 = Util$AgdaModeVscode.Encode.sum(function (x) {
      if (typeof x === "object") {
        if (x.TAG === "MouseSelect") {
          return {
                  TAG: "Payload",
                  _0: "MouseSelect",
                  _1: Common$AgdaModeVscode.Interval.encode(x._0)
                };
        } else {
          return {
                  TAG: "Payload",
                  _0: "KeyUpdate",
                  _1: x._0
                };
        }
      }
      switch (x) {
        case "BrowseUp" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseUp"
                  };
        case "BrowseDown" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseDown"
                  };
        case "BrowseLeft" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseLeft"
                  };
        case "BrowseRight" :
            return {
                    TAG: "TagOnly",
                    _0: "BrowseRight"
                  };
        case "Escape" :
            return {
                    TAG: "TagOnly",
                    _0: "Escape"
                  };
        
      }
    });

var PromptIMUpdate = {
  decode: decode$8,
  encode: encode$8
};

function toString$4(x) {
  if (typeof x !== "object") {
    if (x === "Initialized") {
      return "Initialized";
    } else {
      return "Destroyed";
    }
  }
  switch (x.TAG) {
    case "InputMethod" :
        return "InputMethod";
    case "PromptIMUpdate" :
        return "PromptIMUpdate";
    case "JumpToTarget" :
        return "JumpToTarget";
    
  }
}

var chan = Chan$AgdaModeVscode.make();

var eventContext = React.createContext(chan);

function makeProps(value, children, param) {
  return {
          value: value,
          children: children
        };
}

var make = eventContext.Provider;

var Provider = {
  makeProps: makeProps,
  make: make
};

var decode$9 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "Destroyed" :
            return {
                    TAG: "TagOnly",
                    _0: "Destroyed"
                  };
        case "Initialized" :
            return {
                    TAG: "TagOnly",
                    _0: "Initialized"
                  };
        case "InputMethod" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(decode$7, (function (payload) {
                            return {
                                    TAG: "InputMethod",
                                    _0: payload
                                  };
                          }))
                  };
        case "JumpToTarget" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(Link$AgdaModeVscode.decode, (function (link) {
                            return {
                                    TAG: "JumpToTarget",
                                    _0: link
                                  };
                          }))
                  };
        case "PromptIMUpdate" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(decode$8, (function (payload) {
                            return {
                                    TAG: "PromptIMUpdate",
                                    _0: payload
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventFromView] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var encode$9 = Util$AgdaModeVscode.Encode.sum(function (x) {
      if (typeof x !== "object") {
        if (x === "Initialized") {
          return {
                  TAG: "TagOnly",
                  _0: "Initialized"
                };
        } else {
          return {
                  TAG: "TagOnly",
                  _0: "Destroyed"
                };
        }
      }
      switch (x.TAG) {
        case "InputMethod" :
            return {
                    TAG: "Payload",
                    _0: "InputMethod",
                    _1: encode$7(x._0)
                  };
        case "PromptIMUpdate" :
            return {
                    TAG: "Payload",
                    _0: "PromptIMUpdate",
                    _1: encode$8(x._0)
                  };
        case "JumpToTarget" :
            return {
                    TAG: "Payload",
                    _0: "JumpToTarget",
                    _1: Link$AgdaModeVscode.encode(x._0)
                  };
        
      }
    });

var EventFromView = {
  InputMethod: InputMethod$1,
  PromptIMUpdate: PromptIMUpdate,
  toString: toString$4,
  chan: chan,
  eventContext: eventContext,
  Provider: Provider,
  decode: decode$9,
  encode: encode$9
};

var decode$10 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "Event" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(decode$9, (function (payload) {
                            return {
                                    TAG: "Event",
                                    _0: payload
                                  };
                          }))
                  };
        case "Response" :
            return {
                    TAG: "Payload",
                    _0: Json_Decode$JsonCombinators.map(decode$6, (function (payload) {
                            return {
                                    TAG: "Response",
                                    _0: payload
                                  };
                          }))
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[ResponseOrEventFromView] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

var encode$10 = Util$AgdaModeVscode.Encode.sum(function (x) {
      if (x.TAG === "Response") {
        return {
                TAG: "Payload",
                _0: "Response",
                _1: encode$6(x._0)
              };
      } else {
        return {
                TAG: "Payload",
                _0: "Event",
                _1: encode$9(x._0)
              };
      }
    });

var ResponseOrEventFromView = {
  decode: decode$10,
  encode: encode$10
};

exports.Header = Header;
exports.Prompt = Prompt;
exports.Body = Body;
exports.EventToView = EventToView;
exports.$$Request = $$Request;
exports.RequestOrEventToView = RequestOrEventToView;
exports.$$Response = $$Response;
exports.EventFromView = EventFromView;
exports.ResponseOrEventFromView = ResponseOrEventFromView;
/* decode Not a pure module */
