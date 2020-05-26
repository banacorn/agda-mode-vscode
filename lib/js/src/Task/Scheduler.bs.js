// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var $$Promise = require("reason-promise/lib/js/src/js/promise.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Task$AgdaModeVscode = require("./Task.bs.js");
var Caml_chrome_debugger = require("bs-platform/lib/js/caml_chrome_debugger.js");
var State$AgdaModeVscode = require("../State.bs.js");
var Handle__View$AgdaModeVscode = require("./Handle__View.bs.js");
var Handle__Error$AgdaModeVscode = require("./Handle__Error.bs.js");
var Handle__Command$AgdaModeVscode = require("./Handle__Command.bs.js");
var Handle__Response$AgdaModeVscode = require("./Handle__Response.bs.js");

function make(execute) {
  var match = $$Promise.pending(undefined);
  return {
          queue: [],
          status: /* Idle */1,
          execute: execute,
          terminationPromise: match[0],
          terminate: match[1],
          shouldTerminate: false
        };
}

function run(self) {
  var match = self.status;
  if (!match) {
    return $$Promise.resolved(undefined);
  }
  var nextTasks = self.queue.shift();
  return $$Promise.tap(nextTasks !== undefined ? (self.status = /* Busy */0, $$Promise.flatMap($$Promise.tap(Curry._1(self.execute, nextTasks), (function (param) {
                            self.status = /* Idle */1;
                            
                          })), (function (param) {
                        return run(self);
                      }))) : $$Promise.resolved(undefined), (function (param) {
                if (self.shouldTerminate) {
                  return Curry._1(self.terminate, undefined);
                }
                
              }));
}

function push(self, x) {
  self.queue.push(x);
  return run(self);
}

function pushMany(self, xs) {
  self.queue = xs.concat(self.queue);
  return run(self);
}

function terminate(self) {
  var match = self.status;
  if (match) {
    return Curry._1(self.terminate, undefined);
  } else {
    self.shouldTerminate = true;
    return ;
  }
}

var Runner = {
  make: make,
  run: run,
  push: push,
  pushMany: pushMany,
  terminate: terminate
};

function Impl(Editor) {
  var ErrorHandler = Handle__Error$AgdaModeVscode.Impl(Editor);
  var ViewHandler = Handle__View$AgdaModeVscode.Impl(Editor);
  var CommandHandler = Handle__Command$AgdaModeVscode.Impl(Editor);
  var ResponseHandler = Handle__Response$AgdaModeVscode.Impl(Editor);
  var Task = Task$AgdaModeVscode.Impl(Editor);
  var State = State$AgdaModeVscode.Impl(Editor);
  var dispatchCommand = push;
  var sendRequest = function (state, request) {
    var derivedRequests = {
      contents: []
    };
    var runner = make((function (task) {
            return runTask(state, task);
          }));
    var handle = {
      contents: undefined
    };
    var handler = function (error) {
      if (error.tag) {
        var tasks = Curry._1(ErrorHandler.handle, /* Connection */Caml_chrome_debugger.variant("Connection", 0, [error[0]]));
        return $$Promise.get(pushMany(runner, Belt_List.toArray(tasks)), (function (param) {
                      return terminate(runner);
                    }));
      }
      var match = error[0];
      if (!match) {
        return terminate(runner);
      }
      var error$1 = match[0];
      if (error$1.tag) {
        var tasks$1 = Curry._1(ErrorHandler.handle, /* Parser */Caml_chrome_debugger.variant("Parser", 1, [error$1[0]]));
        return $$Promise.get(pushMany(runner, Belt_List.toArray(tasks$1)), (function (param) {
                      return terminate(runner);
                    }));
      }
      var otherTasks = Belt_Array.keep(Belt_List.toArray(Curry._1(ResponseHandler.handle, error$1[0])), (function (req) {
              if (typeof req === "number") {
                return true;
              }
              if (req.tag) {
                return true;
              }
              derivedRequests.contents.push(req[0]);
              return false;
            }));
      pushMany(runner, otherTasks);
      
    };
    var promise = runner.terminationPromise;
    return $$Promise.map($$Promise.tap($$Promise.flatMap(Curry._2(State.sendRequest, state, request), (function (connection) {
                          if (connection.tag) {
                            var tasks = Curry._1(ErrorHandler.handle, connection[0]);
                            return $$Promise.flatMap(runTasks(state, tasks), (function (param) {
                                          return promise;
                                        }));
                          }
                          handle.contents = Curry._1(connection[0].emitter.on, handler);
                          return promise;
                        })), (function (param) {
                      return Belt_Option.forEach(handle.contents, (function (f) {
                                    return Curry._1(f, undefined);
                                  }));
                    })), (function (param) {
                  return derivedRequests.contents;
                }));
  };
  var sendRequests = function (state, requests) {
    if (!requests) {
      return $$Promise.resolved(undefined);
    }
    var xs = requests[1];
    return $$Promise.flatMap(sendRequest(state, requests[0]), (function (xs$prime) {
                  return sendRequests(state, Belt_List.concat(Belt_List.fromArray(xs$prime), xs));
                }));
  };
  var runTask = function (state, task) {
    if (typeof task === "number") {
      console.log("[ task ][ terminate ] ");
      return Curry._1(State.destroy, state);
    }
    switch (task.tag | 0) {
      case /* SendRequest */0 :
          console.log("[ task ][ send request ]");
          return sendRequests(state, /* :: */Caml_chrome_debugger.simpleVariant("::", [
                        task[0],
                        /* [] */0
                      ]));
      case /* ViewReq */1 :
          console.log("< >");
          return Curry._2(State.sendRequestToView, state, task[0]);
      case /* ViewRes */2 :
          var tasks = Curry._1(ViewHandler.handle, task[0]);
          return runTasks(state, tasks);
      case /* WithState */3 :
          return $$Promise.flatMap(Curry._1(task[0], state), (function (param) {
                        return runTasks(state, param);
                      }));
      
    }
  };
  var runTasks = function (state, tasks) {
    if (!tasks) {
      return $$Promise.resolved(undefined);
    }
    var xs = tasks[1];
    return $$Promise.flatMap(runTask(state, tasks[0]), (function (param) {
                  return runTasks(state, xs);
                }));
  };
  var make$1 = function (state) {
    return make((function (command) {
                  var tasks = Curry._1(CommandHandler.handle, command);
                  return runTasks(state, tasks);
                }));
  };
  var destroy = function (runner) {
    return runner.terminationPromise;
  };
  return {
          ErrorHandler: ErrorHandler,
          ViewHandler: ViewHandler,
          CommandHandler: CommandHandler,
          ResponseHandler: ResponseHandler,
          Task: Task,
          State: State,
          dispatchCommand: dispatchCommand,
          sendRequest: sendRequest,
          sendRequests: sendRequests,
          runTask: runTask,
          runTasks: runTasks,
          make: make$1,
          destroy: destroy
        };
}

exports.Runner = Runner;
exports.Impl = Impl;
/* Promise Not a pure module */