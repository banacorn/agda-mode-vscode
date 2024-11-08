// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_dict = require("rescript/lib/js/js_dict.js");
var Js_array = require("rescript/lib/js/js_array.js");
var Js_string = require("rescript/lib/js/js_string.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Agda$AgdaModeVscode = require("../../../Agda.bs.js");
var Item$AgdaModeVscode = require("../../Component/Item.bs.js");
var Util$AgdaModeVscode = require("../../../Util/Util.bs.js");
var Common$AgdaModeVscode = require("../../Common.bs.js");
var Parser$AgdaModeVscode = require("../../../Parser/Parser.bs.js");
var RichText$AgdaModeVscode = require("../../Component/RichText.bs.js");
var Emacs__Parser$AgdaModeVscode = require("./Emacs__Parser.bs.js");

function partiteMetas(xs) {
  return Emacs__Parser$AgdaModeVscode.Dict.split(xs, "metas", (function (rawMetas) {
                var metas = Emacs__Parser$AgdaModeVscode.aggregateLines(rawMetas);
                var indexOfHiddenMetas = Belt_Array.getIndexBy(metas, (function (s) {
                        return Belt_Option.isSome(Agda$AgdaModeVscode.Output.parseOutputWithLocation(s));
                      }));
                return Emacs__Parser$AgdaModeVscode.Dict.partite(metas, (function (param) {
                              var i = param[1];
                              if (indexOfHiddenMetas !== undefined) {
                                if (i === indexOfHiddenMetas) {
                                  return "hiddenMetas";
                                } else if (i === 0) {
                                  return "interactionMetas";
                                } else {
                                  return ;
                                }
                              } else if (i === 0) {
                                return "interactionMetas";
                              } else {
                                return ;
                              }
                            }));
              }));
}

function partiteWarningsOrErrors(xs, key) {
  return Emacs__Parser$AgdaModeVscode.Dict.update(xs, key, (function (raw) {
                var partial_arg = /^\u2014{4}/;
                var hasDelimeter = Belt_Option.isSome(Belt_Option.flatMap(Belt_Array.get(raw, 0), (function (param) {
                            return Js_string.match_(partial_arg, param);
                          })));
                var lines = hasDelimeter ? Js_array.sliceFrom(1, raw) : raw;
                var markWarningStart = function (line) {
                  return Belt_Option.isSome(Common$AgdaModeVscode.AgdaRange.parse(line));
                };
                var glueBack = function (xs) {
                  var partial_arg = /at$/;
                  return Belt_Option.isSome(Belt_Option.flatMap(Belt_Array.get(xs, xs.length - 1 | 0), (function (param) {
                                    return Js_string.match_(partial_arg, param);
                                  })));
                };
                return Belt_Array.map(Emacs__Parser$AgdaModeVscode.Array_.mergeWithNext(Emacs__Parser$AgdaModeVscode.Array_.partite(lines, markWarningStart), glueBack), Util$AgdaModeVscode.$$String.unlines);
              }));
}

function parseError(raw) {
  var lines = Parser$AgdaModeVscode.splitToLines(raw);
  var partial_arg = /^\u2014{4} Error/;
  var hasBothErrorsAndWarnings = Belt_Option.isSome(Belt_Option.flatMap(Belt_Array.get(lines, 0), (function (param) {
              return Js_string.match_(partial_arg, param);
            })));
  var markWarningStart = function (line) {
    return Belt_Option.isSome(Common$AgdaModeVscode.AgdaRange.parse(line));
  };
  var glueBack = function (xs) {
    var partial_arg = /at$/;
    return Belt_Option.isSome(Belt_Option.flatMap(Belt_Array.get(xs, xs.length - 1 | 0), (function (param) {
                      return Js_string.match_(partial_arg, param);
                    })));
  };
  if (!hasBothErrorsAndWarnings) {
    return Emacs__Parser$AgdaModeVscode.Dict.update(Emacs__Parser$AgdaModeVscode.Dict.partite(lines, (function (param) {
                      if (param[1] === 0) {
                        return "errors";
                      }
                      
                    })), "errors", (function (xs) {
                  return [Util$AgdaModeVscode.$$String.unlines(xs)];
                }));
  }
  var isWarning = function (line) {
    return Belt_Option.isSome(Js_string.match_(/^\u2014{4} Warning\(s\)/, line));
  };
  var predicate = function (param) {
    if (param[1] === 0) {
      return "errors";
    } else if (isWarning(param[0])) {
      return "warnings";
    } else {
      return ;
    }
  };
  return Emacs__Parser$AgdaModeVscode.Dict.update(Emacs__Parser$AgdaModeVscode.Dict.update(Emacs__Parser$AgdaModeVscode.Dict.partite(lines, predicate), "errors", (function (xs) {
                    return [Util$AgdaModeVscode.$$String.unlines(xs.slice(1))];
                  })), "warnings", (function (xs) {
                return Belt_Array.map(Emacs__Parser$AgdaModeVscode.Array_.mergeWithNext(Emacs__Parser$AgdaModeVscode.Array_.partite(xs.slice(1), markWarningStart), glueBack), Util$AgdaModeVscode.$$String.unlines);
              }));
}

function parseGoalType(raw) {
  var markGoal = function (param) {
    return Belt_Option.map(Js_string.match_(/^Goal:/, param[0]), (function (param) {
                  return "goal";
                }));
  };
  var markHave = function (param) {
    return Belt_Option.map(Js_string.match_(/^Have:/, param[0]), (function (param) {
                  return "have";
                }));
  };
  var markMetas = function (param) {
    return Belt_Option.map(Js_string.match_(/\u2014{60}/g, param[0]), (function (param) {
                  return "metas";
                }));
  };
  var partiteGoalTypeContext = function (xs) {
    return Emacs__Parser$AgdaModeVscode.Dict.partite(xs, (function (line) {
                  var v = markGoal(line);
                  if (v !== undefined) {
                    return v;
                  }
                  var v$1 = markHave(line);
                  if (v$1 !== undefined) {
                    return v$1;
                  }
                  var v$2 = markMetas(line);
                  if (v$2 !== undefined) {
                    return v$2;
                  }
                  
                }));
  };
  var removeDelimeter = function (xs) {
    return Emacs__Parser$AgdaModeVscode.Dict.update(xs, "metas", (function (x) {
                  return Js_array.sliceFrom(1, x);
                }));
  };
  var lines = Parser$AgdaModeVscode.splitToLines(raw);
  return partiteMetas(removeDelimeter(partiteGoalTypeContext(lines)));
}

function render(dictionary) {
  return Js_array.concatMany(Belt_Array.map(Js_dict.entries(dictionary), (function (param) {
                    var lines = param[1];
                    switch (param[0]) {
                      case "errors" :
                          return Belt_Array.map(lines, (function (line) {
                                        return Item$AgdaModeVscode.error(RichText$AgdaModeVscode.parse(line), undefined);
                                      }));
                      case "goal" :
                          return Belt_Option.mapWithDefault(Agda$AgdaModeVscode.Expr.parse((function (__x) {
                                              return Js_string.sliceToEnd(5, __x);
                                            })(Util$AgdaModeVscode.$$String.unlines(lines))), [], (function (expr) {
                                        return [{
                                                  TAG: "Labeled",
                                                  _0: "Goal",
                                                  _1: "special",
                                                  _2: Agda$AgdaModeVscode.Expr.render(expr),
                                                  _3: undefined,
                                                  _4: undefined,
                                                  [Symbol.for("name")]: "Labeled"
                                                }];
                                      }));
                      case "have" :
                          return Belt_Option.mapWithDefault(Agda$AgdaModeVscode.Expr.parse((function (__x) {
                                              return Js_string.sliceToEnd(5, __x);
                                            })(Util$AgdaModeVscode.$$String.unlines(lines))), [], (function (expr) {
                                        return [{
                                                  TAG: "Labeled",
                                                  _0: "Have",
                                                  _1: "special",
                                                  _2: Agda$AgdaModeVscode.Expr.render(expr),
                                                  _3: undefined,
                                                  _4: undefined,
                                                  [Symbol.for("name")]: "Labeled"
                                                }];
                                      }));
                      case "hiddenMetas" :
                          return Belt_Array.map(Belt_Array.keepMap(Belt_Array.map(lines, Agda$AgdaModeVscode.Output.parseOutputWithLocation), (function (x) {
                                            return x;
                                          })), (function (output) {
                                        return Agda$AgdaModeVscode.Output.renderItem(output);
                                      }));
                      case "interactionMetas" :
                          return Belt_Array.map(Belt_Array.keepMap(Belt_Array.map(lines, Agda$AgdaModeVscode.Output.parseOutputWithoutLocation), (function (x) {
                                            return x;
                                          })), (function (output) {
                                        return Agda$AgdaModeVscode.Output.renderItem(output);
                                      }));
                      case "metas" :
                          return Belt_Array.map(Belt_Array.keepMap(Belt_Array.map(lines, Agda$AgdaModeVscode.Output.parse), (function (x) {
                                            return x;
                                          })), (function (output) {
                                        return Agda$AgdaModeVscode.Output.renderItem(output);
                                      }));
                      case "warnings" :
                          return Belt_Array.map(lines, (function (line) {
                                        return Item$AgdaModeVscode.warning(RichText$AgdaModeVscode.parse(line), undefined);
                                      }));
                      default:
                        return [];
                    }
                  })), []);
}

function parseAllGoalsWarnings(title, body) {
  var partiteAllGoalsWarnings = function (title, body) {
    var lines = Parser$AgdaModeVscode.splitToLines(body);
    var hasMetas = Belt_Option.isSome((function (__x) {
              return Js_string.match_(/Goals/, __x);
            })(title));
    var hasWarnings = Belt_Option.isSome((function (__x) {
              return Js_string.match_(/Warnings/, __x);
            })(title));
    var hasErrors = Belt_Option.isSome((function (__x) {
              return Js_string.match_(/Errors/, __x);
            })(title));
    var markMetas = function (param) {
      if (hasMetas && param[1] === 0) {
        return "metas";
      }
      
    };
    var markWarnings = function (param) {
      if (hasWarnings) {
        if (hasMetas) {
          return Belt_Option.map((function (__x) {
                          return Js_string.match_(/Warnings/, __x);
                        })((function (__x) {
                              return Js_string.slice(5, 13, __x);
                            })(param[0])), (function (param) {
                        return "warnings";
                      }));
        } else if (param[1] === 0) {
          return "warnings";
        } else {
          return ;
        }
      }
      
    };
    var markErrors = function (param) {
      if (hasErrors) {
        if (hasMetas || hasWarnings) {
          return Belt_Option.map((function (__x) {
                          return Js_string.match_(/Errors/, __x);
                        })((function (__x) {
                              return Js_string.slice(5, 11, __x);
                            })(param[0])), (function (param) {
                        return "errors";
                      }));
        } else if (param[1] === 0) {
          return "errors";
        } else {
          return ;
        }
      }
      
    };
    return Emacs__Parser$AgdaModeVscode.Dict.partite(lines, (function (line) {
                  var value = markMetas(line);
                  if (value !== undefined) {
                    return value;
                  }
                  var value$1 = markWarnings(line);
                  if (value$1 !== undefined) {
                    return value$1;
                  }
                  var value$2 = markErrors(line);
                  if (value$2 !== undefined) {
                    return value$2;
                  }
                  
                }));
  };
  return partiteWarningsOrErrors(partiteWarningsOrErrors(partiteMetas(partiteAllGoalsWarnings(title, body)), "warnings"), "errors");
}

function parseOutputs(raw) {
  var lines = Emacs__Parser$AgdaModeVscode.aggregateLines(Parser$AgdaModeVscode.splitToLines(raw));
  return Belt_Array.map(Belt_Array.keepMap(Belt_Array.map(lines, Agda$AgdaModeVscode.Output.parse), (function (x) {
                    return x;
                  })), (function (output) {
                return Agda$AgdaModeVscode.Output.renderItem(output);
              }));
}

function parseAndRenderTextWithLocation(raw) {
  return [{
            TAG: "Unlabeled",
            _0: RichText$AgdaModeVscode.parse(raw),
            _1: undefined,
            _2: undefined,
            [Symbol.for("name")]: "Unlabeled"
          }];
}

function parseAndRenderSearchAbout(raw) {
  var lines = Parser$AgdaModeVscode.splitToLines(raw);
  var outputs = Belt_Array.map(Belt_Array.keepMap(Belt_Array.map(Emacs__Parser$AgdaModeVscode.aggregateLines(Belt_Array.map((function (__x) {
                            return Js_array.sliceFrom(1, __x);
                          })(lines), (function (param) {
                          return Js_string.sliceToEnd(2, param);
                        }))), Agda$AgdaModeVscode.Output.parse), (function (x) {
              return x;
            })), (function (output) {
          return Agda$AgdaModeVscode.Output.renderItem(output);
        }));
  var target = Belt_Option.map(Belt_Array.get(lines, 0), (function (param) {
          return Js_string.sliceToEnd(18, param);
        }));
  if (target !== undefined) {
    if (outputs.length === 0) {
      return [{
                TAG: "Unlabeled",
                _0: RichText$AgdaModeVscode.parse("There are no definitions about " + target),
                _1: undefined,
                _2: undefined,
                [Symbol.for("name")]: "Unlabeled"
              }];
    } else {
      return Belt_Array.concatMany([
                  [{
                      TAG: "Unlabeled",
                      _0: RichText$AgdaModeVscode.parse("Definitions about " + (target + ":")),
                      _1: undefined,
                      _2: undefined,
                      [Symbol.for("name")]: "Unlabeled"
                    }],
                  outputs
                ]);
    }
  } else {
    return [{
              TAG: "Unlabeled",
              _0: RichText$AgdaModeVscode.parse("Don't know what to search about"),
              _1: undefined,
              _2: undefined,
              [Symbol.for("name")]: "Unlabeled"
            }];
  }
}

exports.partiteMetas = partiteMetas;
exports.partiteWarningsOrErrors = partiteWarningsOrErrors;
exports.parseError = parseError;
exports.parseGoalType = parseGoalType;
exports.render = render;
exports.parseAllGoalsWarnings = parseAllGoalsWarnings;
exports.parseOutputs = parseOutputs;
exports.parseAndRenderTextWithLocation = parseAndRenderTextWithLocation;
exports.parseAndRenderSearchAbout = parseAndRenderSearchAbout;
/* Agda-AgdaModeVscode Not a pure module */
