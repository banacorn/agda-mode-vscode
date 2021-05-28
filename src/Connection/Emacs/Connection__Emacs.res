open Belt

module Error = Connection__Emacs__Error
module Scheduler = Connection__Scheduler

module ProcInfo: {
  type t = {
    path: string,
    args: array<string>,
    version: string,
  }
  let make: (string, array<string>) => Promise.t<result<t, Error.t>>
  let findPath: unit => Promise.t<result<string, Error.t>>
  let toString: t => string
} = {
  type t = {
    path: string,
    args: array<string>,
    version: string,
  }

  @module external untildify: string => string = "untildify"

  // a more sophiscated "make"
  let make = (path, args): Promise.t<result<t, Error.t>> => {
    let validator = (output): result<string, string> =>
      switch Js.String.match_(%re("/Agda version (.*)/"), output) {
      | None => Error("Cannot read Agda version")
      | Some(match_) =>
        switch match_[1] {
        | None => Error("Cannot read Agda version")
        | Some(version) => Ok(version)
        }
      }
    // normailize the path by replacing the tild "~/" with the absolute path of home directory
    let path = untildify(path)
    Connection__Process.Validation.run("\"" ++ (path ++ "\" -V"), validator)
    ->Promise.mapOk(version => {
      path: path,
      args: args,
      version: version,
    })
    ->Promise.mapError(e => Error.Validation(e))
  }

  let findPath = () => {
    // first, get the path from the config (stored in the Editor)
    let storedPath = Config.Connection.getAgdaPath()
    if storedPath == "" || storedPath == "." {
      // if there's no stored path, find one from the OS (with the specified name)
      let agdaVersion = Config.Connection.getAgdaVersion()
      Connection__Process.PathSearch.run(
        agdaVersion,
        "If you know where the executable of Agda is located, please fill it in \"agdaMode.agdaPath\" in the Settings.",
      )
      ->Promise.mapOk(Js.String.trim)
      ->Promise.mapError(e => Error.PathSearch(e))
    } else {
      Promise.resolved(Ok(storedPath))
    }
  }

  // for making error report
  let toString = self => {
    let path = "* path: " ++ self.path
    let args = "* args: " ++ Util.Pretty.array(self.args)
    let version = "* version: " ++ self.version
    let os = "* platform: " ++ N.OS.type_()

    "## Parse Log" ++
    ("\n" ++
    (path ++ ("\n" ++ (args ++ ("\n" ++ (version ++ ("\n" ++ (os ++ "\n"))))))))
  }
}

module type Module = {
  type t
  // lifecycle
  let make: unit => Promise.t<result<t, Error.t>>
  let destroy: t => Promise.t<unit>
  // messaging
  let sendRequest2: (t, string) => unit

  let sendRequest: (
    t,
    string,
    result<Response.t, Error.t> => Promise.t<unit>,
  ) => Promise.t<result<unit, Error.t>>
  let getInfo: t => (string, string)
}

module Module: Module = {
  type t = {
    procInfo: ProcInfo.t,
    process: Connection__Process.t,
    chan: Chan.t<result<Parser.Incr.Gen.t<Response.Prioritized.t>, Error.t>>,
    mutable encountedFirstPrompt: bool,
  }

  let destroy = self => {
    self.chan->Chan.destroy
    self.encountedFirstPrompt = false
    self.process->Connection__Process.destroy
  }

  let wire = (self): unit => {
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
    let toResponse = Parser.Incr.Gen.flatMap(x =>
      switch x {
      | Error((no, e)) => Parser.Incr.Gen.Yield(Error(Parser.Error.SExpression(no, e)))
      | Ok(Parser.SExpression.A("Agda2>")) => Parser.Incr.Gen.Stop
      | Ok(tokens) => Parser.Incr.Gen.Yield(Response.Prioritized.parse(tokens))
      }
    )

    // resolves the requests in the queue
    let handleResponse = (res: Parser.Incr.Gen.t<result<Response.Prioritized.t, Parser.Error.t>>) =>
      switch res {
      | Yield(Ok(response)) => self.chan->Chan.emit(Ok(Yield(response)))
      | Yield(Error(parseError)) => self.chan->Chan.emit(Error(ResponseParseError(parseError)))
      | Stop =>
        if self.encountedFirstPrompt {
          self.chan->Chan.emit(Ok(Stop))
        } else {
          // do nothing when encountering the first Stop
          self.encountedFirstPrompt = true
        }
      }

    // incremental S-expression
    let incrParser = Parser.SExpression.makeIncr(x => x->toResponse->handleResponse)

    // listens to the "data" event on the stdout
    // The chunk may contain various fractions of the Agda output
    // TODO: handle the `listenerHandle`
    let listenerHandle = ref(None)
    listenerHandle :=
      Connection__Process.onOutput(self.process, x =>
        switch x {
        | Stdout(rawText) =>
          // sometimes Agda would return error messages from STDOUT
          if Js.String.startsWith("Error:", rawText) {
            Js.log("stdout: " ++ rawText)
            self.chan->Chan.emit(Error(AgdaError(rawText)))
          } else {
            Js.log("stdout OK: " ++ rawText)
            // split the raw text into pieces and feed it to the parser
            rawText->Parser.split->Array.forEach(Parser.Incr.feed(incrParser))
          }
        | Stderr(e) => Js.log2("stderr: ", e)
        | Event(e) =>
          Js.log2("event: ", Connection__Process.Event.toString(e))
          self.chan->Chan.emit(Error(Process(e)))
        }
      )->Some
  }

  let make = () => {
    // store the path in the editor config
    let persistPathInConfig = (procInfo: ProcInfo.t): Promise.t<result<ProcInfo.t, Error.t>> =>
      Config.Connection.setAgdaPath(procInfo.path)->Promise.map(() => Ok(procInfo))

    // Js.Array.concat([1, 2, 3], [4, 5, 6]) == [4, 5, 6, 1, 2, 3], fuck me right?
    let args = Js.Array.concat(Config.Connection.getCommandLineOptions(), ["--interaction"])

    ProcInfo.findPath()
    ->Promise.flatMapOk(path => {
      ProcInfo.make(path, args)
    })
    ->Promise.flatMapOk(persistPathInConfig)
    ->Promise.mapOk(procInfo => {
      procInfo: procInfo,
      process: Connection__Process.make(procInfo.path, procInfo.args),
      chan: Chan.make(),
      encountedFirstPrompt: false,
    })
    ->Promise.tapOk(wire)
  }

  let sendRequestPrim = (conn, encoded): unit =>
    conn.process->Connection__Process.send(encoded)->ignore

  let sendRequest2 = sendRequestPrim

  let onResponse = (conn, callback) => {
    let scheduler = Scheduler.make()
    // this promise get resolved after all Responses has been received from Agda
    let (promise, stopListener) = Promise.pending()

    // There are 2 kinds of Responses
    //  NonLast Response :
    //    * get handled first
    //    * don't invoke `sendAgdaRequest`
    //  Last Response :
    //    * have priorities, those with the smallest priority number are executed first
    //    * only get handled:
    //        1. after prompt has reappeared
    //        2. after all NonLast Responses
    //        3. after all interactive highlighting is complete
    //    * may invoke `sendAgdaRequest`
    let listener: result<Parser.Incr.Gen.t<Response.Prioritized.t>, Error.t> => unit = x =>
      switch x {
      | Error(error) =>
        // stop the Agda Response listener
        stopListener(Error(error))
        callback(Error(error))->ignore
      | Ok(Yield(NonLast(response))) =>
        scheduler->Scheduler.runNonLast(response => callback(Ok(response)), response)
      | Ok(Yield(Last(priority, response))) => scheduler->Scheduler.addLast(priority, response)
      | Ok(Stop) =>
        // stop the Agda Response listener
        stopListener(Ok())
        // start handling Last Responses, after all NonLast Responses have been handled
        scheduler->Scheduler.runLast(response => callback(Ok(response)))
      }

    // start listening for responses
    let listenerHandle = ref(None)
    listenerHandle := Some(conn.chan->Chan.on(listener))
    // destroy the listener after all responses have been received
    promise->Promise.tap(_ =>
      listenerHandle.contents->Option.forEach(destroyListener => destroyListener())
    )
  }

  let sendRequest = (conn, request, handler): Promise.promise<Promise.result<unit, Error.t>> => {
    // this promise gets resolved after all Responses have been received and handled
    let promise = onResponse(conn, handler)
    sendRequestPrim(conn, request)
    promise
  }

  let getInfo = conn => (conn.procInfo.version, conn.procInfo.path)
}

include Module
