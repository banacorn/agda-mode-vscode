// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Parser$AgdaModeVscode = require("../../../Parser/Parser.bs.js");
var Connection__Transport__Process$AgdaModeVscode = require("../../Transport/Connection__Transport__Process.bs.js");

function toString(x) {
  if (typeof x !== "object") {
    return [
            "Connection Error",
            "Connection via TCP not supported yet"
          ];
  }
  switch (x.TAG) {
    case "Process" :
        return [
                "Process Error",
                Connection__Transport__Process$AgdaModeVscode.$$Event.toString(x._0)
              ];
    case "AgdaError" :
        return [
                "Agda Error",
                x._0
              ];
    case "ResponseParseError" :
        return [
                "Internal Parse Error",
                Parser$AgdaModeVscode.$$Error.toString(x._0)
              ];
    
  }
}

exports.toString = toString;
/* Parser-AgdaModeVscode Not a pure module */
