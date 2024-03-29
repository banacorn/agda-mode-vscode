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
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: 3,
                                    _0: text,
                                    [Symbol.for("name")]: "Error"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "Plain" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: 0,
                                    _0: text,
                                    [Symbol.for("name")]: "Plain"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "Success" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: 1,
                                    _0: text,
                                    [Symbol.for("name")]: "Success"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "Warning" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: 2,
                                    _0: text,
                                    [Symbol.for("name")]: "Warning"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[Header] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

function encode(param) {
  return Util$AgdaModeVscode.Encode.sum((function (x) {
                switch (x.TAG | 0) {
                  case /* Plain */0 :
                      return {
                              TAG: 0,
                              _0: "Plain",
                              _1: x._0,
                              [Symbol.for("name")]: "Payload"
                            };
                  case /* Success */1 :
                      return {
                              TAG: 0,
                              _0: "Success",
                              _1: x._0,
                              [Symbol.for("name")]: "Payload"
                            };
                  case /* Warning */2 :
                      return {
                              TAG: 0,
                              _0: "Warning",
                              _1: x._0,
                              [Symbol.for("name")]: "Payload"
                            };
                  case /* Error */3 :
                      return {
                              TAG: 0,
                              _0: "Error",
                              _1: x._0,
                              [Symbol.for("name")]: "Payload"
                            };
                  
                }
              }), param);
}

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
          body: Json_Encode$JsonCombinators.option((function (prim) {
                  return prim;
                }), param.body),
          placeholder: Json_Encode$JsonCombinators.option((function (prim) {
                  return prim;
                }), param.placeholder),
          value: Json_Encode$JsonCombinators.option((function (prim) {
                  return prim;
                }), param.value)
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
                    TAG: 1,
                    _0: /* Activate */0,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "BrowseDown" :
            return {
                    TAG: 1,
                    _0: /* BrowseDown */4,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "BrowseLeft" :
            return {
                    TAG: 1,
                    _0: /* BrowseLeft */5,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "BrowseRight" :
            return {
                    TAG: 1,
                    _0: /* BrowseRight */3,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "BrowseUp" :
            return {
                    TAG: 1,
                    _0: /* BrowseUp */2,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "Deactivate" :
            return {
                    TAG: 1,
                    _0: /* Deactivate */1,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "Update" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.tuple3(Json_Decode$JsonCombinators.string, Translator$AgdaModeVscode.decode, Json_Decode$JsonCombinators.$$int), (function (param) {
                            return {
                                    _0: param[0],
                                    _1: param[1],
                                    _2: param[2],
                                    [Symbol.for("name")]: "Update"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventToView.InputMethod] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

function encode$2(param) {
  return Util$AgdaModeVscode.Encode.sum((function (x) {
                if (typeof x !== "number") {
                  return {
                          TAG: 0,
                          _0: "Update",
                          _1: Json_Encode$JsonCombinators.tuple3((function (prim) {
                                  return prim;
                                }), Translator$AgdaModeVscode.encode, (function (prim) {
                                  return prim;
                                }), [
                                x._0,
                                x._1,
                                x._2
                              ]),
                          [Symbol.for("name")]: "Payload"
                        };
                }
                switch (x) {
                  case /* Activate */0 :
                      return {
                              TAG: 1,
                              _0: "Activate",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  case /* Deactivate */1 :
                      return {
                              TAG: 1,
                              _0: "Deactivate",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  case /* BrowseUp */2 :
                      return {
                              TAG: 1,
                              _0: "BrowseUp",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  case /* BrowseRight */3 :
                      return {
                              TAG: 1,
                              _0: "BrowseRight",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  case /* BrowseDown */4 :
                      return {
                              TAG: 1,
                              _0: "BrowseDown",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  case /* BrowseLeft */5 :
                      return {
                              TAG: 1,
                              _0: "BrowseLeft",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  
                }
              }), param);
}

var InputMethod = {
  decode: decode$2,
  encode: encode$2
};

function toString$1(x) {
  if (typeof x === "number") {
    return "PromptInterrupt";
  }
  switch (x.TAG | 0) {
    case /* Display */0 :
        return "Display " + x._0._0;
    case /* Append */1 :
        return "Append " + x._0._0;
    case /* SetStatus */2 :
        return "SetStatus " + x._0;
    case /* PromptIMUpdate */3 :
        return "PromptIMUpdate " + x._0;
    case /* InputMethod */4 :
        return "InputMethod";
    
  }
}

var decode$3 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "Append" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.tuple2(decode, Json_Decode$JsonCombinators.array(Item$AgdaModeVscode.decode)), (function (param) {
                            return {
                                    TAG: 1,
                                    _0: param[0],
                                    _1: param[1],
                                    [Symbol.for("name")]: "Append"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "Display" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.tuple2(decode, Json_Decode$JsonCombinators.array(Item$AgdaModeVscode.decode)), (function (param) {
                            return {
                                    TAG: 0,
                                    _0: param[0],
                                    _1: param[1],
                                    [Symbol.for("name")]: "Display"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "InputMethod" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(decode$2, (function (payload) {
                            return {
                                    TAG: 4,
                                    _0: payload,
                                    [Symbol.for("name")]: "InputMethod"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "PromptIMUpdate" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: 3,
                                    _0: text,
                                    [Symbol.for("name")]: "PromptIMUpdate"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "PromptInterrupt" :
            return {
                    TAG: 1,
                    _0: /* PromptInterrupt */0,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "SetStatus" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (text) {
                            return {
                                    TAG: 2,
                                    _0: text,
                                    [Symbol.for("name")]: "SetStatus"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventToView] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

function encode$3(param) {
  return Util$AgdaModeVscode.Encode.sum((function (x) {
                if (typeof x === "number") {
                  return {
                          TAG: 1,
                          _0: "PromptInterrupt",
                          [Symbol.for("name")]: "TagOnly"
                        };
                }
                switch (x.TAG | 0) {
                  case /* Display */0 :
                      return {
                              TAG: 0,
                              _0: "Display",
                              _1: Json_Encode$JsonCombinators.tuple2(encode, (function (param) {
                                      return Json_Encode$JsonCombinators.array(Item$AgdaModeVscode.encode, param);
                                    }), [
                                    x._0,
                                    x._1
                                  ]),
                              [Symbol.for("name")]: "Payload"
                            };
                  case /* Append */1 :
                      return {
                              TAG: 0,
                              _0: "Append",
                              _1: Json_Encode$JsonCombinators.tuple2(encode, (function (param) {
                                      return Json_Encode$JsonCombinators.array(Item$AgdaModeVscode.encode, param);
                                    }), [
                                    x._0,
                                    x._1
                                  ]),
                              [Symbol.for("name")]: "Payload"
                            };
                  case /* SetStatus */2 :
                      return {
                              TAG: 0,
                              _0: "SetStatus",
                              _1: x._0,
                              [Symbol.for("name")]: "Payload"
                            };
                  case /* PromptIMUpdate */3 :
                      return {
                              TAG: 0,
                              _0: "PromptIMUpdate",
                              _1: x._0,
                              [Symbol.for("name")]: "Payload"
                            };
                  case /* InputMethod */4 :
                      return {
                              TAG: 0,
                              _0: "InputMethod",
                              _1: encode$2(x._0),
                              [Symbol.for("name")]: "Payload"
                            };
                  
                }
              }), param);
}

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
                _0: param[0],
                _1: param[1],
                [Symbol.for("name")]: "Prompt"
              };
      }));

function encode$4(request) {
  return Json_Encode$JsonCombinators.tuple2(encode, encode$1, [
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
  if (x.TAG === /* Request */0) {
    return toString$2(x._0);
  } else {
    return toString$1(x._0);
  }
}

var decode$5 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "Event" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(decode$3, (function (payload) {
                            return {
                                    TAG: 1,
                                    _0: payload,
                                    [Symbol.for("name")]: "Event"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "Request" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(decode$4, (function (payload) {
                            return {
                                    TAG: 0,
                                    _0: payload,
                                    [Symbol.for("name")]: "Request"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[RequestOrEventToView] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

function encode$5(param) {
  return Util$AgdaModeVscode.Encode.sum((function (x) {
                if (x.TAG === /* Request */0) {
                  return {
                          TAG: 0,
                          _0: "Request",
                          _1: encode$4(x._0),
                          [Symbol.for("name")]: "Payload"
                        };
                } else {
                  return {
                          TAG: 0,
                          _0: "Event",
                          _1: encode$3(x._0),
                          [Symbol.for("name")]: "Payload"
                        };
                }
              }), param);
}

var RequestOrEventToView = {
  toString: toString$3,
  decode: decode$5,
  encode: encode$5
};

var decode$6 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "PromptInterrupted" :
            return {
                    TAG: 1,
                    _0: /* PromptInterrupted */0,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "PromptSuccess" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (result) {
                            return {
                                    _0: result,
                                    [Symbol.for("name")]: "PromptSuccess"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[Response] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

function encode$6(param) {
  return Util$AgdaModeVscode.Encode.sum((function (x) {
                if (x) {
                  return {
                          TAG: 0,
                          _0: "PromptSuccess",
                          _1: x._0,
                          [Symbol.for("name")]: "Payload"
                        };
                } else {
                  return {
                          TAG: 1,
                          _0: "PromptInterrupted",
                          [Symbol.for("name")]: "TagOnly"
                        };
                }
              }), param);
}

var $$Response = {
  decode: decode$6,
  encode: encode$6
};

var decode$7 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "ChooseSymbol" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function (symbol) {
                            return {
                                    TAG: 1,
                                    _0: symbol,
                                    [Symbol.for("name")]: "ChooseSymbol"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "InsertChar" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function ($$char) {
                            return {
                                    TAG: 0,
                                    _0: $$char,
                                    [Symbol.for("name")]: "InsertChar"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventFromView.InputMethod] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

function encode$7(param) {
  return Util$AgdaModeVscode.Encode.sum((function (x) {
                if (x.TAG === /* InsertChar */0) {
                  return {
                          TAG: 0,
                          _0: "InsertChar",
                          _1: x._0,
                          [Symbol.for("name")]: "Payload"
                        };
                } else {
                  return {
                          TAG: 0,
                          _0: "ChooseSymbol",
                          _1: x._0,
                          [Symbol.for("name")]: "Payload"
                        };
                }
              }), param);
}

var InputMethod$1 = {
  decode: decode$7,
  encode: encode$7
};

var decode$8 = Util$AgdaModeVscode.Decode.sum(function (x) {
      switch (x) {
        case "BrowseDown" :
            return {
                    TAG: 1,
                    _0: /* BrowseDown */1,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "BrowseLeft" :
            return {
                    TAG: 1,
                    _0: /* BrowseLeft */2,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "BrowseRight" :
            return {
                    TAG: 1,
                    _0: /* BrowseRight */3,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "BrowseUp" :
            return {
                    TAG: 1,
                    _0: /* BrowseUp */0,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "Escape" :
            return {
                    TAG: 1,
                    _0: /* Escape */4,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "KeyUpdate" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Json_Decode$JsonCombinators.string, (function ($$char) {
                            return {
                                    TAG: 1,
                                    _0: $$char,
                                    [Symbol.for("name")]: "KeyUpdate"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "MouseSelect" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Common$AgdaModeVscode.Interval.decode, (function (interval) {
                            return {
                                    TAG: 0,
                                    _0: interval,
                                    [Symbol.for("name")]: "MouseSelect"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventFromView.PromptIMUpdate] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

function encode$8(param) {
  return Util$AgdaModeVscode.Encode.sum((function (x) {
                if (typeof x !== "number") {
                  if (x.TAG === /* MouseSelect */0) {
                    return {
                            TAG: 0,
                            _0: "MouseSelect",
                            _1: Common$AgdaModeVscode.Interval.encode(x._0),
                            [Symbol.for("name")]: "Payload"
                          };
                  } else {
                    return {
                            TAG: 0,
                            _0: "KeyUpdate",
                            _1: x._0,
                            [Symbol.for("name")]: "Payload"
                          };
                  }
                }
                switch (x) {
                  case /* BrowseUp */0 :
                      return {
                              TAG: 1,
                              _0: "BrowseUp",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  case /* BrowseDown */1 :
                      return {
                              TAG: 1,
                              _0: "BrowseDown",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  case /* BrowseLeft */2 :
                      return {
                              TAG: 1,
                              _0: "BrowseLeft",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  case /* BrowseRight */3 :
                      return {
                              TAG: 1,
                              _0: "BrowseRight",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  case /* Escape */4 :
                      return {
                              TAG: 1,
                              _0: "Escape",
                              [Symbol.for("name")]: "TagOnly"
                            };
                  
                }
              }), param);
}

var PromptIMUpdate = {
  decode: decode$8,
  encode: encode$8
};

var chan = Chan$AgdaModeVscode.make(undefined);

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
                    TAG: 1,
                    _0: /* Destroyed */1,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "Initialized" :
            return {
                    TAG: 1,
                    _0: /* Initialized */0,
                    [Symbol.for("name")]: "TagOnly"
                  };
        case "InputMethod" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(decode$7, (function (payload) {
                            return {
                                    TAG: 0,
                                    _0: payload,
                                    [Symbol.for("name")]: "InputMethod"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "JumpToTarget" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(Link$AgdaModeVscode.decode, (function (link) {
                            return {
                                    TAG: 2,
                                    _0: link,
                                    [Symbol.for("name")]: "JumpToTarget"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "PromptIMUpdate" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(decode$8, (function (payload) {
                            return {
                                    TAG: 1,
                                    _0: payload,
                                    [Symbol.for("name")]: "PromptIMUpdate"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[EventFromView] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

function encode$9(param) {
  return Util$AgdaModeVscode.Encode.sum((function (x) {
                if (typeof x === "number") {
                  if (x === /* Initialized */0) {
                    return {
                            TAG: 1,
                            _0: "Initialized",
                            [Symbol.for("name")]: "TagOnly"
                          };
                  } else {
                    return {
                            TAG: 1,
                            _0: "Destroyed",
                            [Symbol.for("name")]: "TagOnly"
                          };
                  }
                }
                switch (x.TAG | 0) {
                  case /* InputMethod */0 :
                      return {
                              TAG: 0,
                              _0: "InputMethod",
                              _1: encode$7(x._0),
                              [Symbol.for("name")]: "Payload"
                            };
                  case /* PromptIMUpdate */1 :
                      return {
                              TAG: 0,
                              _0: "PromptIMUpdate",
                              _1: encode$8(x._0),
                              [Symbol.for("name")]: "Payload"
                            };
                  case /* JumpToTarget */2 :
                      return {
                              TAG: 0,
                              _0: "JumpToTarget",
                              _1: Link$AgdaModeVscode.encode(x._0),
                              [Symbol.for("name")]: "Payload"
                            };
                  
                }
              }), param);
}

var EventFromView = {
  InputMethod: InputMethod$1,
  PromptIMUpdate: PromptIMUpdate,
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
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(decode$9, (function (payload) {
                            return {
                                    TAG: 1,
                                    _0: payload,
                                    [Symbol.for("name")]: "Event"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        case "Response" :
            return {
                    TAG: 0,
                    _0: Json_Decode$JsonCombinators.map(decode$6, (function (payload) {
                            return {
                                    TAG: 0,
                                    _0: payload,
                                    [Symbol.for("name")]: "Response"
                                  };
                          })),
                    [Symbol.for("name")]: "Payload"
                  };
        default:
          throw {
                RE_EXN_ID: Json_Decode$JsonCombinators.DecodeError,
                _1: "[ResponseOrEventFromView] Unknown constructor: " + x,
                Error: new Error()
              };
      }
    });

function encode$10(param) {
  return Util$AgdaModeVscode.Encode.sum((function (x) {
                if (x.TAG === /* Response */0) {
                  return {
                          TAG: 0,
                          _0: "Response",
                          _1: encode$6(x._0),
                          [Symbol.for("name")]: "Payload"
                        };
                } else {
                  return {
                          TAG: 0,
                          _0: "Event",
                          _1: encode$9(x._0),
                          [Symbol.for("name")]: "Payload"
                        };
                }
              }), param);
}

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
