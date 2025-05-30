// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Util$AgdaModeVscode = require("./Util/Util.bs.js");

function make() {
  var match = Util$AgdaModeVscode.Promise_.pending();
  return {
          state: {
            TAG: "Pending",
            _0: match[0],
            _1: match[1],
            [Symbol.for("name")]: "Pending"
          }
        };
}

async function get(self) {
  var value = self.state;
  if (value.TAG === "Pending") {
    return await value._0;
  } else {
    return value._0;
  }
}

function set(self, value) {
  var match = self.state;
  if (match.TAG !== "Pending") {
    self.state = {
      TAG: "Acquired",
      _0: value,
      [Symbol.for("name")]: "Acquired"
    };
    return ;
  }
  match._1(value);
  self.state = {
    TAG: "Acquired",
    _0: value,
    [Symbol.for("name")]: "Acquired"
  };
}

var Module = {
  make: make,
  get: get,
  set: set
};

exports.Module = Module;
exports.make = make;
exports.get = get;
exports.set = set;
/* Util-AgdaModeVscode Not a pure module */
