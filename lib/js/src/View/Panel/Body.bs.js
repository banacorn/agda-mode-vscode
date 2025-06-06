// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var React = require("react");
var Item$AgdaModeVscode = require("../Component/Item.bs.js");
var JsxPPXReactSupportU = require("rescript/lib/js/jsxPPXReactSupportU.js");

function Body(props) {
  var items = props.items;
  if (items.length !== 0) {
    return React.createElement("div", {
                className: "agda-mode-body"
              }, React.createElement("ul", undefined, items.map(function (item, i) {
                        return JsxPPXReactSupportU.createElementWithKey(String(i), Item$AgdaModeVscode.make, {
                                    item: item
                                  });
                      })));
  } else {
    return React.createElement(React.Fragment, {});
  }
}

var make = Body;

exports.make = make;
/* react Not a pure module */
