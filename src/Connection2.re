open Belt;

module Process = AgdaMode.Process;
module Error = {
  type t =
    | PathSearch(Process.PathSearch.Error.t)
    | Validation(Process.Validation.Error.t)
    | Process(Process.Error.t);
  let toString =
    fun
    | PathSearch(e) => Process.PathSearch.Error.toString(e)
    | Validation(e) => Process.Validation.Error.toString(e)
    | Process(e) => Process.Error.toString(e);
};

module Metadata = {
  /* supported protocol */
  module Protocol = {
    type t =
      | EmacsOnly
      | EmacsAndJSON;
    let toString =
      fun
      | EmacsOnly => "Emacs"
      | EmacsAndJSON => "Emacs / JSON";
  };

  type t = {
    path: string,
    args: array(string),
    version: string,
    protocol: Protocol.t,
  };

  let serialize = self => {
    let path = "* path: " ++ self.path;
    let args = "* args: " ++ Util.Pretty.array(self.args);
    let version = "* version: " ++ self.version;
    let protocol = "* protocol: " ++ Protocol.toString(self.protocol);
    let os = "* platform: " ++ N.OS.type_();

    {j|## Parse Log
$path
$args
$version
$protocol
$os
  |j};
  };
};

type response = Parser.Incr.Event.t(result(Response.t, Parser.Error.t));

type t = {
  metadata: Metadata.t,
  process: Process.t,
  mutable queue: list(Event.t(result(response, Process.Error.t))),
  mutable resetLogOnLoad: bool,
  mutable encountedFirstPrompt: bool,
  //   mutable log: Log.t,
};

let disconnect = (error, self) => {
  self.process.disconnect() |> ignore;
  self.queue->List.forEach(ev => ev.Event.emit(Error(error)));
  self.queue = [];
  self.encountedFirstPrompt = false;
  //   self.log = [||];
};

let autoSearch = name =>
  Process.PathSearch.run(name)->Promise.mapError(e => Error.PathSearch(e));

// a more sophiscated "make"
let validateAndMake = (path, args): Promise.t(result(Metadata.t, Error.t)) => {
  let validator = (output): result((string, Metadata.Protocol.t), string) => {
    switch (Js.String.match([%re "/Agda version (.*)/"], output)) {
    | None => Error("Cannot read Agda version")
    | Some(match) =>
      switch (match[1]) {
      | None => Error("Cannot read Agda version")
      | Some(version) =>
        Ok((
          version,
          Js.Re.test_([%re "/--interaction-json/"], output)
            ? Metadata.Protocol.EmacsAndJSON : Metadata.Protocol.EmacsOnly,
        ))
      }
    };
  };

  Process.Validation.run(path ++ " -V", validator)
  ->Promise.mapOk(((version, protocol)) =>
      {Metadata.path, args, version, protocol}
    )
  ->Promise.mapError(e => Error.Validation(e));
};

let connect = (metadata: Metadata.t): t => {
  let args = [|"--interaction"|] |> Array.concat(metadata.args);
  let process = Process.make(metadata.path, args);

  {
    metadata,
    process,
    queue: [],
    resetLogOnLoad: true,
    encountedFirstPrompt: false,
    // log: [||],
  };
};

let wire = (self): t => {
  //   let logSExpression =
  //     Parser.Incr.Event.tap(
  //       fun
  //       | Error(_) => ()
  //       | Ok(expr) => Log.logSExpression(expr, self.log),
  //     );

  // We use the prompt "Agda2>" as the delimiter of the end of a response
  // However, the prompt "Agda2>" also appears at the very start of the conversation
  // So this would be what it looks like:
  //    >>> request
  //      stop          <------- wierd stop
  //      yield
  //      yield
  //      stop
  //    >> request
  //      yield
  //      yield
  //      stop
  //
  let toResponse =
    Parser.Incr.Event.flatMap(
      fun
      | Error(parseError) => Parser.Incr.Event.Yield(Error(parseError))
      | Ok(Parser.SExpression.A("Agda2>")) => Parser.Incr.Event.Stop
      | Ok(tokens) => Parser.Incr.Event.Yield(Response.parse(tokens)),
    );

  //   let logResponse =
  //     Parser.Incr.Event.tap(
  //       fun
  //       | Error(_) => ()
  //       | Ok(expr) => Log.logResponse(expr, self.log),
  //     );

  // resolves the requests in the queue
  let handleResponse = (res: response) => {
    switch (self.queue) {
    | [] => ()
    | [req, ...rest] =>
      switch (res) {
      | Yield(x) => req.emit(Ok(Yield(x)))
      | Stop =>
        if (self.encountedFirstPrompt) {
          // pop the queue on Stop
          req.emit(Ok(Stop));
          self.queue = rest;
          req.Event.destroy() |> ignore;
        } else {
          // do nothing when encountering the first Stop
          self.encountedFirstPrompt =
            true;
        }
      }
    };
  };

  let mapError = x =>
    Parser.Incr.Event.map(
      fun
      | Ok(x) => Ok(x)
      | Error((no, e)) => Error(Parser.Error.SExpression(no, e)),
      x,
    );

  let pipeline =
    Parser.SExpression.makeIncr(x => x->mapError->toResponse->handleResponse);
  //   x->logSExpression->toResponse->logResponse->handleResponse

  // listens to the "data" event on the stdout
  // The chunk may contain various fractions of the Agda output
  let onData: result(string, Process.Error.t) => unit =
    fun
    | Ok(rawText) => {
        // store the raw text in the log
        // Log.logRawText(rawText, self.log);
        // split the raw text into pieces and feed it to the parser
        rawText
        ->Parser.split
        ->Array.forEach(Parser.Incr.feed(pipeline));
      }
    | Error(e) => {
        // emit error to all of the request in the queue
        self.queue
        ->List.forEach(req => {
            req.Event.emit(Error(e));
            req.destroy();
          });
        // clean the queue
        self.queue = [];
      };

  self.process.emitter.on(onData) |> ignore;

  self;
};

let send = (request, self): Event.t(result(response, Process.Error.t)) => {
  let reqEvent = Event.make();

  self.queue = [reqEvent, ...self.queue];

  self.process.send(request) |> ignore;

  reqEvent;
};

let resetLog = self => {
  //   self.log = [||];
};

let dump = self => {
  let serialize = self => {
    // let metadata = self.metadata |> Metadata.serialize;
    // let log = self.log |> Log.serialize;
    // metadata ++ "\n" ++ log ++ "\n";
    ();
    "";
  };
  let text = serialize(self);
  let itemOptions = {
    "initialLine": 0,
    "initialColumn": 0,
    "split": "left",
    "activatePane": true,
    "activateItem": true,
    "pending": false,
    "searchAllPanes": true,
    "location": (None: option(string)),
  };
  let itemURI = "agda-mode://log.md";
  ();
  //   Atom.Workspace.open_(itemURI, itemOptions)
  //   ->Promise.Js.fromBsPromise
  //   ->Promise.Js.toResult
  //   ->Promise.map(
  //       fun
  //       | Error(_) => ()
  //       | Ok(newItem) => {
  //           newItem |> Atom.TextEditor.insertText(text) |> ignore;
  //         },
  //     )
  //   |> ignore;
};