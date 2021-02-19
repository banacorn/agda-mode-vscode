open Belt

// TODO: sort these errors out
module Error = {
  type t =
    // probing "agda" or "als"
    | CannotConnectViaStdIO(AgdaModeVscode.Process.PathSearch.Error.t)
    | CannotConnectViaTCP(Js.Exn.t)
    | PathSearch(Process.PathSearch.Error.t)
    | Validation(Process.Validation.Error.t)
    | Process(Process.Error.t)
    // LSP related
    | ConnectionError(Js.Exn.t)
    | CannotSendRequest(Js.Exn.t)
    // Encoding / Decoding
    | CannotDecodeResponse(string, Js.Json.t)
    | CannotDecodeNotification(string, Js.Json.t)
    | InitializationFailed
    // TODO: refactor these
    | LSPCannotConnectDevServer
    | LSPInternalError(string)
    | LSPConnection(Js.Exn.t)
    | LSPSendRequest(Js.Exn.t)
    | LSPClientCannotDecodeResponse(string, Js.Json.t)
    | CannotDecodeRequest(string)
    | ResponseParseError(Parser.Error.t)
    | NotConnectedYet
  let toString = x =>
    switch x {
    | CannotConnectViaStdIO(e) =>
      let (_header, body) = AgdaModeVscode.Process.PathSearch.Error.toString(e)
      ("Cannot locate \"als\"", body ++ "\nPlease make sure that the executable is in the path")
    | CannotConnectViaTCP(_) => (
        "Cannot connect with the server",
        "Please enter \":main -d\" in ghci",
      )
    | PathSearch(e) => Process.PathSearch.Error.toString(e)
    | Validation(e) => Process.Validation.Error.toString(e)
    | Process(e) => Process.Error.toString(e)
    | ConnectionError(exn) =>
      let isECONNREFUSED =
        Js.Exn.message(exn)->Option.mapWithDefault(
          false,
          Js.String.startsWith("connect ECONNREFUSED"),
        )

      isECONNREFUSED
        ? ("LSP Connection Error", "Please enter \":main -d\" in ghci")
        : ("LSP Client Error", Js.Exn.message(exn)->Option.getWithDefault(""))
    | CannotSendRequest(exn) => (
        "PANIC: Cannot send request",
        "Please file an issue\n" ++ Js.Exn.message(exn)->Option.getWithDefault(""),
      )
    | CannotDecodeResponse(msg, json) => (
        "PANIC: Cannot decode response",
        "Please file an issue\n\n" ++ msg ++ "\n" ++ Json.stringify(json),
      )
    | CannotDecodeNotification(msg, json) => (
        "PANIC: Cannot decode Notification",
        "Please file an issue\n\n" ++ msg ++ "\n" ++ Json.stringify(json),
      )
    | CannotDecodeRequest(e) => ("LSP: Server Cannot Decode Request", e)
    | InitializationFailed => ("LSP: InitializationFailed", "")

    | LSPCannotConnectDevServer => ("LSP: Cannot Connect to the Dev Server", "")
    | LSPInternalError(e) => ("LSP: Internal Error", e)
    | LSPConnection(e) => ("LSP: Connection Failed", Util.JsError.toString(e))
    | LSPSendRequest(e) => ("LSP: Cannot Send Request", Util.JsError.toString(e))
    | LSPClientCannotDecodeResponse(e, json) => (
        "LSP: Client Cannot Decode Response",
        e ++ "\n" ++ Js.Json.stringify(json),
      )
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
  type method = ViaStdIO(string, string) | ViaTCP(int)

  module LSPReq = {
    type t = Initialize | Command(string)

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Initialize => object_(list{("tag", string("ReqInitialize"))})
      | Command(raw) => object_(list{("tag", string("ReqCommand")), ("contents", raw |> string)})
      }
  }

  type version = string
  module LSPRes = {
    type t =
      | Initialize(version)
      | CommandDone
      | ServerCannotDecodeRequest(string)

    let fromJsError = (error: 'a): string => %raw("function (e) {return e.toString()}")(error)

    open Json.Decode
    open Util.Decode
    let decode: decoder<t> = sum(x =>
      switch x {
      | "ResInitialize" => Contents(string |> map(version => Initialize(version)))
      | "ResCommandDone" => TagOnly(CommandDone)
      | "ResCannotDecodeRequest" =>
        Contents(string |> map(version => ServerCannotDecodeRequest(version)))
      | tag => raise(DecodeError("[LSP.Response] Unknown constructor: " ++ tag))
      }
    )
  }

  module LSPNtf = {
    type t =
      | Response(string)
      | ResponseEnd

    let toString = x =>
      switch x {
      | Response(s) => s
      | ResponseEnd => "========"
      }

    open Json.Decode
    open Util.Decode
    let decode: decoder<t> = sum(x =>
      switch x {
      | "NtfResponse" => Contents(string |> map(version => Response(version)))
      | "NtfResponseEnd" => TagOnly(ResponseEnd)
      | tag => raise(DecodeError("[LSP.Notification] Unknown constructor: " ++ tag))
      }
    )
  }

  module type Module = {
    // lifecycle
    let start: bool => Promise.t<result<(version, method), Error.t>>
    let stop: unit => Promise.t<unit>
    // messaging
    let sendRequest: (string, string => unit) => Promise.t<result<unit, Error.t>>
    let getVersion: unit => option<version>
    let onError: (Error.t => unit) => VSCode.Disposable.t
    // predicate
    let isConnected: unit => bool
  }

  module Module: Module = {
    module Client = {
      open VSCode

      type t = {
        client: LSP.LanguageClient.t,
        subscription: VSCode.Disposable.t,
        method: method,
      }

      // for emitting errors
      let errorChan: Chan.t<Js.Exn.t> = Chan.make()
      // for emitting data
      let dataChan: Chan.t<Js.Json.t> = Chan.make()

      let onError = callback =>
        errorChan->Chan.on(e => callback(Error.ConnectionError(e)))->VSCode.Disposable.make
      let onResponse = callback => dataChan->Chan.on(callback)->VSCode.Disposable.make

      let sendRequest = (self, data) =>
        self.client
        ->LSP.LanguageClient.onReady
        ->Promise.Js.toResult
        ->Promise.flatMapOk(() => {
          self.client->LSP.LanguageClient.sendRequest("agda", data)->Promise.Js.toResult
        })
        ->Promise.mapError(exn => Error.CannotSendRequest(exn))

      let destroy = self => {
        self.subscription->VSCode.Disposable.dispose->ignore
        self.client->LSP.LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
      }

      let make = method => {
        // let emittedError = ref(false)

        let serverOptions = switch method {
        | ViaTCP(port) => LSP.ServerOptions.makeWithStreamInfo(port)
        | ViaStdIO(name, _path) => LSP.ServerOptions.makeWithCommand(name)
        }

        let clientOptions = {
          // Register the server for plain text documents
          let documentSelector: DocumentSelector.t = [
            StringOr.others({
              open DocumentFilter
              {
                scheme: Some("file"),
                pattern: None,
                language: Some("agda"),
              }
            }),
          ]

          // Notify the server about file changes to '.clientrc files contained in the workspace
          let synchronize: FileSystemWatcher.t = Workspace.createFileSystemWatcher(
            %raw("'**/.clientrc'"),
            ~ignoreCreateEvents=false,
            ~ignoreChangeEvents=false,
            ~ignoreDeleteEvents=false,
          )

          let errorHandler: LSP.ErrorHandler.t = LSP.ErrorHandler.make(
            ~error=(exn, _msg, _count) => {
              errorChan->Chan.emit(exn)
              Shutdown
            },
            ~closed=() => {
              DoNotRestart
            },
          )
          LSP.LanguageClientOptions.make(documentSelector, synchronize, errorHandler)
        }

        // Create the language client
        let languageClient = LSP.LanguageClient.make(
          "agdaLanguageServer",
          "Agda Language Server",
          serverOptions,
          clientOptions,
        )

        let self = {
          client: languageClient,
          subscription: languageClient->LSP.LanguageClient.start,
          method: method,
        }

        // Let `LanguageClient.onReady` and `errorChan->Chan.once` race
        Promise.race(list{
          self.client->LSP.LanguageClient.onReady->Promise.Js.toResult,
          errorChan->Chan.once->Promise.map(err => Error(err)),
        })
        ->Promise.map(result =>
          switch result {
          | Error(error) => Error(error)
          | Ok() =>
            self.client->LSP.LanguageClient.onRequest("agda", json => {
              dataChan->Chan.emit(json)
              Promise.resolved()
            })
            Ok(self)
          }
        )
        ->Promise.mapError(e => Error.ConnectionError(e))
      }
    }

    // for internal bookkeeping
    type state =
      | Disconnected
      | Connected(Client.t, version)
    // internal state singleton
    let singleton: ref<state> = ref(Disconnected)

    // stop the LSP client
    let stop = () =>
      switch singleton.contents {
      | Disconnected => Promise.resolved()
      | Connected(client, _version) =>
        // update the status
        singleton := Disconnected
        // destroy the client
        client->Client.destroy
      }

    // catches exceptions occured when decoding JSON values
    let decodeResponse = (json: Js.Json.t): result<LSPRes.t, Error.t> =>
      switch LSPRes.decode(json) {
      | response => Ok(response)
      | exception Json.Decode.DecodeError(msg) => Error(Error.CannotDecodeResponse(msg, json))
      }

    let decodeNotification = (json: Js.Json.t): result<LSPNtf.t, Error.t> =>
      switch LSPNtf.decode(json) {
      | notification => Ok(notification)
      | exception Json.Decode.DecodeError(msg) => Error(Error.CannotDecodeNotification(msg, json))
      }

    // let onResponse = handler => Client.onResponse(json => handler(decodeResponse(json)))
    let onError = Client.onError

    let sendRequestPrim = (client, request): Promise.t<result<LSPRes.t, Error.t>> => {
      client
      ->Client.sendRequest(LSPReq.encode(request))
      ->Promise.flatMapOk(json => Promise.resolved(decodeResponse(json)))
    }

    // see if the server is available
    let probe = (tryTCP, port, name) => {
      // see if "als" is available
      let probeStdIO = name => {
        AgdaModeVscode.Process.PathSearch.run(name)
        ->Promise.mapOk(path => ViaStdIO(name, Js.String.trim(path)))
        ->Promise.mapError(e => Error.CannotConnectViaStdIO(e))
      }
      // see if the TCP port is available
      let probeTCP = port => {
        let (promise, resolve) = Promise.pending()
        // connect and resolve `Ok()`` on success
        let socket = N.Net.connect(port, () => resolve(Ok()))
        // resolve `Error(CannotConnect(Js.Exn.t))` on error
        socket
        ->N.Net.Socket.on(#error(exn => resolve(Error(Error.CannotConnectViaTCP(exn)))))
        ->ignore
        // destroy the connection afterwards
        promise->Promise.mapOk(() => {
          N.Net.Socket.destroy(socket)->ignore
          ViaTCP(port)
        })
      }
      if tryTCP {
        probeTCP(port)->Promise.flatMapError(_ => probeStdIO(name))
      } else {
        probeStdIO(name)
      }
    }

    // start the LSP client
    let start = tryTCP =>
      switch singleton.contents {
      | Disconnected =>
        probe(tryTCP, 4000, "als")
        ->Promise.flatMapOk(Client.make)
        ->Promise.flatMap(result =>
          switch result {
          | Error(error) =>
            singleton.contents = Disconnected
            Promise.resolved(Error(error))
          | Ok(client) =>
            // send `ReqInitialize` and wait for `ResInitialize` before doing anything else
            sendRequestPrim(client, Initialize)->Promise.flatMapOk(response =>
              switch response {
              | ServerCannotDecodeRequest(msg) =>
                Promise.resolved(Error(Error.CannotDecodeRequest(msg)))
              | CommandDone => Promise.resolved(Error(Error.InitializationFailed))
              | Initialize(version) =>
                // update the status
                singleton.contents = Connected(client, version)
                Promise.resolved(Ok((version, client.method)))
              }
            )
          }
        )
      | Connected(client, version) => Promise.resolved(Ok((version, client.method)))
      }

    let getVersion = () =>
      switch singleton.contents {
      | Disconnected => None
      | Connected(_, version) => Some(version)
      }

    let isConnected = () =>
      switch singleton.contents {
      | Disconnected => false
      | Connected(_, _version) => true
      }

    let sendRequest = (request, notificationHandler) =>
      switch singleton.contents {
      | Connected(client, _version) =>
        // waits for `ResponseEnd`
        let (promise, resolve) = Promise.pending()

        // listens for notifications
        let subscription = Client.onResponse(json => {
          switch decodeNotification(json) {
          | Ok(Response(responese)) => notificationHandler(responese)
          | Ok(ResponseEnd) => resolve(Ok())
          | Error(error) => resolve(Error(error))
          }
        })

        let stopListeningForNotifications = () => subscription->VSCode.Disposable.dispose->ignore

        // sends `Command` and waits for `ResponseEnd`
        sendRequestPrim(client, Command(request))
        ->Promise.flatMapOk(result =>
          switch result {
          | ServerCannotDecodeRequest(e) => Promise.resolved(Error(Error.CannotDecodeRequest(e)))
          | Initialize(_) => Promise.resolved(Error(Error.InitializationFailed))
          // waits for `ResponseEnd`
          | CommandDone => promise
          }
        )
        // stop listening for notifications
        ->Promise.tap(_ => stopListeningForNotifications())
      | Disconnected => Promise.resolved(Error(Error.NotConnectedYet))
      }
  }
  include Module
}
