module Error = Connection__Target__Agda__Error
module Scheduler = Connection__Scheduler
module Process = Connection__Process

module type Module = {
  type t
  // lifecycle
  let make: (Connection__Target__IPC.t, string, string) => promise<result<t, Error.t>>
  let destroy: t => promise<unit>
  // messaging
  let sendRequest: (t, string, Response.t => promise<unit>) => promise<result<unit, Error.t>>
  let getInfo: t => (string, string) // version and path
}

module Module: Module = {
  type t = {
    process: Process.t,
    version: string,
    path: string,
    chan: Chan.t<result<Parser.Incr.Gen.t<Response.Prioritized.t>, Error.t>>,
    mutable encountedFirstPrompt: bool,
  }

  let destroy = self => {
    self.chan->Chan.destroy
    self.encountedFirstPrompt = false
    self.process->Process.destroy
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
      , ...)

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
      Process.onOutput(self.process, x =>
        switch x {
        | Stdout(rawText) =>
          // sometimes Agda would return error messages from STDOUT
          if rawText->String.startsWith("Error:") {
            self.chan->Chan.emit(Error(AgdaError(rawText)))
          } else {
            // split the raw text into pieces and feed it to the parser
            rawText->Parser.splitToLines->Array.forEach(Parser.Incr.feed(incrParser, ...))
          }
        | Stderr(error) =>
          // sometimes Agda would return error messages from STDOUT
          self.chan->Chan.emit(Error(AgdaError(error)))
        | Event(e) => self.chan->Chan.emit(Error(Process(e)))
        }
      )->Some
  }

  let make = async (method, version, path) =>
    switch method {
    | Connection__Target__IPC.ViaTCP(_) => Error(Error.ConnectionViaTCPNotSupported)
    | ViaPipe(_, _, _) =>
      let args = Array.concat(["--interaction"], Config.Connection.getCommandLineOptions())
      let conn = {
        process: Process.make(path, args),
        version,
        path,
        chan: Chan.make(),
        encountedFirstPrompt: false,
      }
      wire(conn)
      Ok(conn)
    }

  let onResponse = async (conn, callback) => {
    let scheduler = Scheduler.make()
    // this promise get resolved after all Responses has been received from Agda
    let (responsePromise, stopResponseListener, _) = Util.Promise_.pending()

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
        stopResponseListener(Error(error))
      | Ok(Yield(NonLast(response))) =>
        scheduler->Scheduler.runNonLast(response => callback(response), response)
      | Ok(Yield(Last(priority, response))) => scheduler->Scheduler.addLast(priority, response)
      | Ok(Stop) =>
        // start handling Last Responses, after all NonLast Responses have been handled
        // resolve the `responseHandlingPromise` after all Last Responses have been handled
        scheduler
        ->Scheduler.runLast(response => callback(response))
        ->Promise.finally(() =>
          // stop the Agda Response listener
          stopResponseListener(Ok())
        )
        ->Promise.done
      }

    // start listening for responses
    let listenerHandle = ref(None)
    listenerHandle := Some(conn.chan->Chan.on(listener))
    // destroy the listener after all responses have been received
    let result = await responsePromise
    listenerHandle.contents->Option.forEach(destroyListener => destroyListener())
    result
  }

  let sendRequest = (conn, request, handler): promise<result<unit, Error.t>> => {
    // this promise gets resolved after all Responses have been received and handled
    let promise = onResponse(conn, handler)
    Process.send(conn.process, request)->ignore
    promise
  }

  let getInfo = conn => (conn.version, conn.path)
}

include Module
