// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var React = require("react");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Item$AgdaModeVscode = require("../Component/Item.bs.js");

function Body(Props) {
  var items = Props.items;
  if (items.length !== 0) {
    return React.createElement("div", {
                className: "agda-mode-body"
              }, React.createElement("ul", undefined, Belt_Array.mapWithIndex(items, (function (i, item) {
                          return React.createElement(Item$AgdaModeVscode.make, {
                                      item: item,
                                      key: String(i)
                                    });
                        }))));
  } else {
    return React.createElement(React.Fragment, undefined);
  }
}

var make = Body;

exports.make = make;
/* react Not a pure module */
