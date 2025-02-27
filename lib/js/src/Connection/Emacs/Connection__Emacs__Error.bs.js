// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Parser$AgdaModeVscode = require("../../Parser/Parser.bs.js");
var Client__Process$LanguageServerMule = require("language-server-mule/lib/js/src/Client/Client__Process.bs.js");

function toString(x) {
  if (typeof x !== "object") {
    return [
            "Connection Error",
            "Connection via TCP not supported yet"
          ];
  }
  switch (x.TAG) {
    case "Validation" :
        return [
                "Validation Error",
                x._0
              ];
    case "Process" :
        return [
                "Process Error",
                Client__Process$LanguageServerMule.$$Event.toString(x._0)
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

var Process;

exports.Process = Process;
exports.toString = toString;
/* Parser-AgdaModeVscode Not a pure module */
