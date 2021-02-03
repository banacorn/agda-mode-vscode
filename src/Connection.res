open Belt

// TODO: sort these errors out
module Error = {
  type t =
    | PathSearch(Process.PathSearch.Error.t)
    | Validation(Process.Validation.Error.t)
    | Process(Process.Error.t)
    | LSPCannotConnectDevServer
    | LSPInternalError(string)
    | LSPConnection(Js.Exn.t)
    | LSPSendRequest(Js.Exn.t)
    | LSPClientCannotDecodeResponse(string, Js.Json.t)
    | LSPServerCannotDecodeRequest(string)
    | ResponseParseError(Parser.Error.t)
    | NotConnectedYet
  let toString = x =>
    switch x {
    | PathSearch(e) => Process.PathSearch.Error.toString(e)
    | Validation(e) => Process.Validation.Error.toString(e)
    | Process(e) => Process.Error.toString(e)
    | LSPCannotConnectDevServer => ("LSP: Cannot Connect to the Dev Server", "")
    | LSPInternalError(e) => ("LSP: Internal Error", e)
    | LSPConnection(e) => ("LSP: Connection Failed", Util.JsError.toString(e))
    | LSPSendRequest(e) => ("LSP: Cannot Send Request", Util.JsError.toString(e))
    | LSPClientCannotDecodeResponse(e, json) => (
        "LSP: Client Cannot Decode Response",
        e ++ "\n" ++ Js.Json.stringify(json),
      )
    | LSPServerCannotDecodeRequest(e) => ("LSP: Server Cannot Decode Request", e)
    // | LSPCannotDecodeRequest(e) => ("LSP: Cannot Decode Request", e)
    | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
    | NotConnectedYet => ("Connection not established yet", "")
    }
}

module type Emacs = {
  type t
  let make: unit => Promise.t<result<t, Error.t>>
  let destroy: t => Promise.t<unit>
  let onResponse: (t, result<Response.t, Error.t> => Promise.t<unit>) => Promise.t<unit>
  let sendRequest: (t, VSCode.TextDocument.t, Request.t) => unit
}

module Emacs: Emacs = {
  // This module makes sure that Last Responses are handled after NonLast Responses
  module Lock: {
    let runNonLast: Promise.t<'a> => unit
    let onceDone: unit => Promise.t<unit>
  } = {
    // keep the number of running NonLast Response
    let tally = ref(0)
    let allDone = Chan.make()
    // NonLast Responses should fed here
    let runNonLast = promise => {
      tally := tally.contents + 1
      promise->Promise.get(_ => {
        tally := tally.contents - 1
        if tally.contents == 0 {
          allDone->Chan.emit()
        }
      })
    }
    // gets resolved once there's no NonLast Responses running
    let onceDone = () =>
      if tally.contents == 0 {
        Promise.resolved()
      } else {
        allDone->Chan.once
      }
  }

  @bs.module external untildify: string => string = "untildify"

  module Metadata = {
    type t = {
      path: string,
      args: array<string>,
      version: string,
    }

    // for making error report
    let _toString = self => {
      let path = "* path: " ++ self.path
      let args = "* args: " ++ Util.Pretty.array(self.args)
      let version = "* version: " ++ self.version
      let os = "* platform: " ++ N.OS.type_()

      "## Parse Log" ++
      ("\n" ++
      (path ++ ("\n" ++ (args ++ ("\n" ++ (version ++ ("\n" ++ (os ++ "\n"))))))))
    }

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
      Process.Validation.run("\"" ++ (path ++ "\" -V"), validator)
      ->Promise.mapOk(version => {
        path: path,
        args: args,
        version: version,
      })
      ->Promise.mapError(e => Error.Validation(e))
    }
  }

  type response = Parser.Incr.Gen.t<result<Response.Prioritized.t, Parser.Error.t>>

  type t = {
    metadata: Metadata.t,
    process: Process.t,
    chan: Chan.t<result<response, Error.t>>,
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
      | Error(parseError) => Parser.Incr.Gen.Yield(Error(parseError))
      | Ok(Parser.SExpression.A("Agda2>")) => Parser.Incr.Gen.Stop
      | Ok(tokens) => Parser.Incr.Gen.Yield(Response.Prioritized.parse(tokens))
      }
    )

    // resolves the requests in the queue
    let handleResponse = (res: response) =>
      switch res {
      | Yield(x) => self.chan->Chan.emit(Ok(Yield(x)))
      | Stop =>
        if self.encountedFirstPrompt {
          self.chan->Chan.emit(Ok(Stop))
        } else {
          // do nothing when encountering the first Stop
          self.encountedFirstPrompt = true
        }
      }

    let mapError = x => Parser.Incr.Gen.map(x =>
        switch x {
        | Ok(x) => Ok(x)
        | Error((no, e)) => Error(Parser.Error.SExpression(no, e))
        }
      , x)

    let pipeline = Parser.SExpression.makeIncr(x => x->mapError->toResponse->handleResponse)

    // listens to the "data" event on the stdout
    // The chunk may contain various fractions of the Agda output
    // TODO: handle the destructor
    let _destructor = self.process->Process.onOutput(x =>
      switch x {
      | Stdout(rawText) =>
        // split the raw text into pieces and feed it to the parser
        rawText->Parser.split->Array.forEach(Parser.Incr.feed(pipeline))
      | Stderr(_) => ()
      | Error(e) => self.chan->Chan.emit(Error(Process(e)))
      }
    )
  }

  let make = () => {
    let getPath = (): Promise.t<result<string, Error.t>> => {
      // first, get the path from the config (stored in the Editor)
      let storedPath = Config.getAgdaPath()
      if storedPath == "" || storedPath == "." {
        // if there's no stored path, find one from the OS (with the specified name)
        let agdaVersion = Config.getAgdaVersion()
        Process.PathSearch.run(agdaVersion)
        ->Promise.mapOk(Js.String.trim)
        ->Promise.mapError(e => Error.PathSearch(e))
      } else {
        Promise.resolved(Ok(storedPath))
      }
    }

    // store the path in the editor config
    let setPath = (metadata: Metadata.t): Promise.t<result<Metadata.t, Error.t>> =>
      Config.setAgdaPath(metadata.path)->Promise.map(() => Ok(metadata))

    let args = ["--interaction"]

    getPath()
    ->Promise.flatMapOk(path => {
      Metadata.make(path, args)
    })
    ->Promise.flatMapOk(setPath)
    ->Promise.mapOk(metadata => {
      metadata: metadata,
      process: Process.make(metadata.path, metadata.args),
      chan: Chan.make(),
      encountedFirstPrompt: false,
    })
    ->Promise.tapOk(wire)
  }

  let sendRequest = (connection, document, request): unit => {
    let filepath = document->VSCode.TextDocument.fileName->Parser.filepath
    let libraryPath = Config.getLibraryPath()
    let highlightingMethod = Config.getHighlightingMethod()
    let backend = Config.getBackend()
    let encoded = Request.encode(
      document,
      connection.metadata.version,
      filepath,
      backend,
      libraryPath,
      highlightingMethod,
      request,
    )
    connection.process->Process.send(encoded)->ignore
  }

  let onResponse = (connection, callback) => {
    // deferred responses are queued here
    let deferredLastResponses: array<(int, Response.t)> = []

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
    let listener: result<response, Error.t> => unit = x =>
      switch x {
      | Error(error) => callback(Error(error))->ignore
      | Ok(Parser.Incr.Gen.Yield(Error(error))) =>
        callback(Error(ResponseParseError(error)))->ignore
      | Ok(Yield(Ok(NonLast(response)))) => Lock.runNonLast(callback(Ok(response)))
      | Ok(Yield(Ok(Last(priority, response)))) =>
        Js.Array.push((priority, response), deferredLastResponses)->ignore
      | Ok(Stop) =>
        // sort the deferred Responses by priority (ascending order)
        let deferredLastResponses =
          Js.Array.sortInPlaceWith(
            (x, y) => compare(fst(x), fst(y)),
            deferredLastResponses,
          )->Array.map(snd)

        // insert `CompleteHighlightingAndMakePromptReappear` handling Last Responses
        Js.Array.unshift(
          Response.CompleteHighlightingAndMakePromptReappear,
          deferredLastResponses,
        )->ignore

        // wait until all NonLast Responses are handled
        Lock.onceDone()
        // stop the Agda Response listener
        ->Promise.tap(stopListener)
        // start handling Last Responses
        ->Promise.map(() => deferredLastResponses->Array.map(res => callback(Ok(res))))
        ->Promise.flatMap(Util.oneByOne)
        ->ignore
      }

    let listenerHandle = ref(None)
    // start listening for responses
    listenerHandle := Some(connection.chan->Chan.on(listener))
    // destroy the listener after all responses have been received
    promise->Promise.tap(() =>
      listenerHandle.contents->Option.forEach(destroyListener => destroyListener())
    )
  }
}

module LSP = {
  module LSPReq = {
    type t = Initialize | Payload(string)

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Initialize => object_(list{("tag", string("ReqInitialize"))})
      | Payload(raw) => object_(list{("tag", string("ReqPayload")), ("contents", raw |> string)})
      }
  }

  type version = string
  module LSPRes = {
    type t =
      | Initialize(version)
      | Payload(string)
      | ServerCannotDecodeRequest(string)

    let fromJsError = (error: 'a): string => %raw("function (e) {return e.toString()}")(error)

    open Json.Decode
    open Util.Decode
    let decode: decoder<t> = sum(x =>
      switch x {
      | "ResInitialize" => Contents(string |> map(version => Initialize(version)))
      | "ResPayload" => Contents(string |> map(payload => Payload(payload)))
      | "ResCannotDecodeRequest" =>
        Contents(string |> map(version => ServerCannotDecodeRequest(version)))
      | tag => raise(DecodeError("[LSP.Response] Unknown constructor: " ++ tag))
      }
    )
  }

  module type Module = {
    module Handle: {
      type t
      let toString: t => string
    }

    // probe the machine to find the language server
    let find: bool => Promise.t<result<Handle.t, Error.t>>

    let start: Handle.t => Promise.t<result<version, Error.t>>
    let stop: unit => Promise.t<unit>
    let sendRequest: string => Promise.t<result<string, Error.t>>
    // let changeMethod: LSP.method => Promise.t<result<option<version>, Error.t>>
    let getVersion: unit => option<version>
    // predicate
    let isConnected: unit => bool
    // output
    let onError: (Js.Exn.t => unit) => VSCode.Disposable.t
    let onChangeStatus: (LSP.status => unit) => VSCode.Disposable.t
    let onChangeMethod: (LSP.method => unit) => VSCode.Disposable.t
  }

  module Module: Module = {
    module Handle = {
      type t = StdIO(string) | TCP(int)
      // for debugging
      let toString = x =>
        switch x {
        | TCP(port) => "on port \"" ++ string_of_int(port) ++ "\""
        | StdIO(path) => "on path \"" ++ path ++ "\""
        }
    }

    // for emitting events
    let statusChan: Chan.t<LSP.status> = Chan.make()
    let methodChan: Chan.t<LSP.method> = Chan.make()

    // for internal bookkeeping
    type state =
      | Disconnected
      | Connected(LSP.Client.t, version)

    // internal states
    type singleton = {
      mutable state: state,
      mutable handle: option<Handle.t>,
    }
    let singleton: singleton = {
      state: Disconnected,
      handle: None,
    }

    // locate the languege server
    let find = devMode =>
      if devMode {
        let port = 4000
        let (promise, resolve) = Promise.pending()
        // connect to port 4000, resolve `Ok()`` on success
        let socket = N.Net.connect(port, () => resolve(Ok()))
        // resolve `Error(LSPCannotConnectDevServer)` on error
        socket
        ->N.Net.Socket.on(#error(_exn => resolve(Error(Error.LSPCannotConnectDevServer))))
        ->ignore
        // destroy the connection afterwards
        promise->Promise.mapOk(() => {
          N.Net.Socket.destroy(socket)->ignore
          Handle.TCP(port)
        })
      } else {
        Process.PathSearch.run("als")
        ->Promise.mapOk(path => Handle.StdIO(Js.String.trim(path)))
        ->Promise.mapError(e => Error.PathSearch(e))
      }

    // stop the LSP client
    let stop = () =>
      switch singleton.state {
      | Disconnected => Promise.resolved()
      | Connected(client, _version) =>
        // update the status
        singleton.state = Disconnected
        statusChan->Chan.emit(Disconnected)
        // destroy the client
        client->LSP.Client.destroy
      }

    let sendRequestPrim = (client, request): Promise.t<result<LSPRes.t, Error.t>> => {
      client
      ->LSP.Client.sendRequest(LSPReq.encode(request))
      ->Promise.map(x =>
        switch x {
        | Ok(json) =>
          switch LSPRes.decode(json) {
          | response => Ok(response)
          | exception Json.Decode.DecodeError(msg) =>
            Error(Error.LSPClientCannotDecodeResponse(msg, json))
          }
        | Error(exn) =>
          statusChan->Chan.emit(Disconnected)
          Error(Error.LSPSendRequest(exn))
        }
      )
    }

    // make and start the LSP client
    let rec startWithHandle = handle =>
      switch singleton.state {
      | Disconnected =>
        let (devMode, method) = switch handle {
        | Handle.TCP(_) => (true, LSP.ViaTCP)
        | StdIO(_) => (true, ViaStdIO)
        }

        LSP.Client.make(devMode, method)->Promise.flatMap(result =>
          switch result {
          | Error(exn) =>
            let isECONNREFUSED =
              Js.Exn.message(exn)->Option.mapWithDefault(
                false,
                Js.String.startsWith("connect ECONNREFUSED"),
              )
            let shouldSwitchToStdIO = isECONNREFUSED && method == ViaTCP

            singleton.state = Disconnected
            statusChan->Chan.emit(Disconnected)
            singleton.handle = None

            if shouldSwitchToStdIO {
              Js.log("Connecting via TCP failed, trying to switch to StdIO")

              find(false)->Promise.flatMapOk(handle => {
                singleton.handle = Some(handle)
                startWithHandle(handle)
              })
            } else {
              Promise.resolved(Error(Error.LSPConnection(exn)))
            }
          | Ok(client) =>
            // send `ReqInitialize` and wait for `ResInitialize` before doing anything else
            sendRequestPrim(client, Initialize)->Promise.flatMapOk(response =>
              switch response {
              | ServerCannotDecodeRequest(msg) =>
                Promise.resolved(Error(Error.LSPServerCannotDecodeRequest(msg)))
              | Payload(_) =>
                Promise.resolved(
                  Error(Error.LSPConnection(Js.Exn.raiseError("Got payload instead"))),
                )
              | Initialize(version) =>
                // update the status
                singleton.state = Connected(client, version)
                statusChan->Chan.emit(Connected)
                Promise.resolved(Ok(version))
              }
            )
          }
        )
      | Connected(_, version) => Promise.resolved(Ok(version))
      }

    // make and start the LSP client
    let start = handle => {
      singleton.handle = Some(handle)
      startWithHandle(handle)
    }

    let getVersion = () =>
      switch singleton.state {
      | Disconnected => None
      | Connected(_, version) => Some(version)
      }

    let isConnected = () =>
      switch singleton.state {
      | Disconnected => false
      | Connected(_, _version) => true
      }

    // let onResponse = handler => Client.onData(json => handler(decodeResponse(json)))
    let onError = LSP.Client.onError
    let onChangeStatus = callback => statusChan->Chan.on(callback)->VSCode.Disposable.make
    let onChangeMethod = callback => methodChan->Chan.on(callback)->VSCode.Disposable.make

    let sendRequest = request =>
      switch singleton.state {
      | Connected(client, _version) =>
        sendRequestPrim(client, Payload(request))->Promise.flatMapOk(result =>
          switch result {
          | Initialize(_) =>
            Promise.resolved(Error(Error.LSPInternalError("Got Initialize when expecting Payload")))
          | Payload(json) => Promise.resolved(Ok(json))
          | ServerCannotDecodeRequest(e) =>
            Promise.resolved(Error(Error.LSPServerCannotDecodeRequest(e)))
          }
        )
      | Disconnected => Promise.resolved(Error(Error.NotConnectedYet))
      }

    // let changeMethod = method => {
    //   // update the state and reconfigure the connection
    //   let (changed, devMode) = switch singleton.handle {
    //   | None => true
    //   | Some(TCP(_)) => method != LSP.ViaTCP
    //   | Some(StdIO(_)) => method != LSP.ViaStdIO
    //   }

    //   if changed {
    //     singleton.handle = method
    //     methodChan->Chan.emit(method)
    //     stop()
    //     ->Promise.flatMap(() => start(singleton.devMode))
    //     ->Promise.mapOk(version => Some(version))
    //   } else {
    //     Promise.resolved(Ok(None))
    //   }
    // }
  }
  include Module
}
