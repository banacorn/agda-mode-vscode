// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var React = require("react");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Webapi__Dom__Element = require("rescript-webapi/lib/js/src/Webapi/Dom/Webapi__Dom__Element.bs.js");

function Prompt(props) {
  var prompt = props.prompt;
  if (prompt === undefined) {
    return React.createElement(React.Fragment, {});
  }
  var body = prompt[0];
  var onUpdatePromptIM = props.onUpdatePromptIM;
  var onSubmit = props.onSubmit;
  var inputMethodActivated = props.inputMethodActivated;
  var placeholder = Belt_Option.getWithDefault(prompt[1], "");
  var value = Belt_Option.getWithDefault(prompt[2], "");
  var match = React.useState(function () {
        return false;
      });
  var setHasFocused = match[1];
  var hasFocused = match[0];
  var match$1 = React.useState(function () {
        
      });
  var setSelectionInterval = match$1[1];
  var selectionInterval = match$1[0];
  var onKeyUp = function ($$event) {
    var match = $$event.key;
    var arrowKey;
    switch (match) {
      case "ArrowDown" :
          arrowKey = "BrowseDown";
          break;
      case "ArrowLeft" :
          arrowKey = "BrowseLeft";
          break;
      case "ArrowRight" :
          arrowKey = "BrowseRight";
          break;
      case "ArrowUp" :
          arrowKey = "BrowseUp";
          break;
      case "Escape" :
          arrowKey = "Escape";
          break;
      default:
        arrowKey = undefined;
    }
    Belt_Option.forEach(arrowKey, (function (action) {
            if (inputMethodActivated) {
              onUpdatePromptIM(action);
              $$event.preventDefault();
              return ;
            } else if (action === "Escape") {
              onSubmit(undefined);
              $$event.preventDefault();
              return ;
            } else {
              return ;
            }
          }));
  };
  var onMouseUp = function ($$event) {
    if (!inputMethodActivated) {
      return ;
    }
    $$event.persist();
    var selectionInterval_0 = $$event.target.selectionStart;
    var selectionInterval_1 = $$event.target.selectionEnd;
    var selectionInterval = [
      selectionInterval_0,
      selectionInterval_1
    ];
    setSelectionInterval(function (param) {
          return selectionInterval;
        });
    onUpdatePromptIM({
          TAG: "MouseSelect",
          _0: selectionInterval,
          [Symbol.for("name")]: "MouseSelect"
        });
  };
  var onChange = function ($$event) {
    var value = $$event.target.value;
    $$event.persist();
    setSelectionInterval(function (param) {
          return [
                  $$event.target.selectionStart,
                  $$event.target.selectionEnd
                ];
        });
    onUpdatePromptIM({
          TAG: "KeyUpdate",
          _0: value,
          [Symbol.for("name")]: "KeyUpdate"
        });
  };
  var inputRef = React.useRef(null);
  React.useEffect(function () {
        Belt_Option.forEach(Caml_option.nullable_to_opt(inputRef.current), (function (input) {
                Belt_Option.forEach(selectionInterval, (function (param) {
                        var setSelectionRange = ((elem, start, end_) => elem.setSelectionRange(start, end_));
                        setSelectionRange(input, param[0], param[1]);
                      }));
              }));
        setTimeout((function () {
                if (!hasFocused) {
                  Belt_Option.forEach(Belt_Option.flatMap(Caml_option.nullable_to_opt(inputRef.current), Webapi__Dom__Element.asHtmlElement), (function (prim) {
                          prim.focus();
                        }));
                  setHasFocused(function (param) {
                        return true;
                      });
                }
                
              }), 100);
      });
  var onSubmit$1 = function (_event) {
    onUpdatePromptIM("Escape");
    onSubmit(value);
  };
  return React.createElement("div", {
              className: "agda-mode-prompt"
            }, React.createElement("form", {
                  onSubmit: onSubmit$1
                }, body !== undefined ? React.createElement("p", undefined, body) : React.createElement(React.Fragment, {}), React.createElement("input", {
                      ref: Caml_option.some(inputRef),
                      placeholder: placeholder,
                      type: "text",
                      value: value,
                      onKeyUp: onKeyUp,
                      onChange: onChange,
                      onMouseUp: onMouseUp
                    })));
}

var make = Prompt;

exports.make = make;
/* react Not a pure module */
