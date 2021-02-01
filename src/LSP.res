open VSCode

module Message = {
  type t = {jsonrpc: string}
}

module ErrorAction = {
  type t = Continue | Shutdown
  type raw = int
  let toEnum = x =>
    switch x {
    | Continue => 1
    | Shutdown => 2
    }
}

module CloseAction = {
  type t = DoNotRestart | Restart
  type raw = int
  let toEnum = x =>
    switch x {
    | DoNotRestart => 1
    | Restart => 2
    }
}

module ErrorHandler: {
  type t
  let make: (
    ~error: (Js.Exn.t, option<Message.t>, option<int>) => ErrorAction.t,
    ~closed: unit => CloseAction.t,
  ) => t
  let makeDefault: (string, int) => t
} = {
  type t = {
    error: (Js.Exn.t, option<Message.t>, option<int>) => ErrorAction.raw,
    closed: unit => CloseAction.raw,
  }

  let make = (~error, ~closed) => {
    let error = (a, b, c) => error(a, b, c)->ErrorAction.toEnum
    let closed = () => closed()->CloseAction.toEnum
    {
      error: error,
      closed: closed,
    }
  }

  // https://github.com/microsoft/vscode-languageserver-node/blob/20681d7632bb129def0c751be73cf76bd01f2f3a/client/src/common/client.ts#L275
  let makeDefault = (name, maxRestartCount) => {
    let restarts = []
    make(
      ~error=(_, _, count) =>
        switch count {
        | Some(count) =>
          if count <= 3 {
            Continue
          } else {
            Shutdown
          }
        | None => Shutdown
        },
      ~closed=() => {
        Js.Array.push(Js.Date.now(), restarts)->ignore
        let length = Js.Array.length(restarts)
        if length <= maxRestartCount {
          Restart
        } else {
          open Belt
          let diff =
            restarts[length - 1]->Option.flatMap(latest =>
              restarts[0]->Option.map(first => latest -. first)
            )
          switch diff {
          | Some(diff) =>
            if int_of_float(diff) <= 3 * 60 * 1000 {
              let max = string_of_int(maxRestartCount + 1)
              Window.showErrorMessage(
                "The " ++
                name ++
                "server crashed " ++
                max ++ " times in the last 3 minutes. The server will not be restarted.",
                [],
              )->ignore
              DoNotRestart
            } else {
              Js.Array.shift(restarts)->ignore
              Restart
            }
          | None => Restart
          }
        }
      },
    )
  }
}

// Options to control the language client
module LanguageClientOptions = {
  type t
  let make: (
    DocumentSelector.t,
    FileSystemWatcher.t,
    ErrorHandler.t,
  ) => t = %raw("function (documentSelector, synchronize, errorHandler) {
      return {
		    documentSelector: documentSelector,
		    synchronize: synchronize,
        errorHandler: errorHandler
      }
    }")
}

// Options to control the language client
module ServerOptions = {
  type t
  let makeWithCommand: string => t = %raw("function (command) {
      return { command: command }
    }")

  let makeWithStreamInfo: int => t = %raw("function (port) {
      const net = require('net');
      const socket = net.createConnection({ port: port })
      return (() => { return new Promise(resolve => resolve({
        writer: socket,
        reader: socket
      })
      )})
    }")
}

module LanguageClient = {
  type t
  // constructor
  @bs.module("vscode-languageclient") @bs.new
  external make: (string, string, ServerOptions.t, LanguageClientOptions.t) => t = "LanguageClient"
  // methods
  @bs.send external start: t => VSCode.Disposable.t = "start"
  @bs.send external stop: t => Promise.Js.t<unit, _> = "stop"
  @bs.send external onReady: t => Promise.Js.t<unit, _> = "onReady"
  @bs.send
  external onNotification: (t, string, 'a => unit) => unit = "onNotification"
  @bs.send
  external sendNotification: (t, string, 'a) => unit = "sendNotification"
  @bs.send
  external sendRequest: (t, string, Js.Json.t) => Promise.Js.t<'result, _> = "sendRequest"
}

open Belt

type status = Disconnected | Connecting | Connected
type method = ViaStdIO | ViaTCP

module type Client = {
  type t

  let onError: (Js.Exn.t => unit) => VSCode.Disposable.t
  let onData: (Js.Json.t => unit) => VSCode.Disposable.t

  let sendRequest: (t, Js.Json.t) => Promise.t<result<Js.Json.t, 'a>>

  let destroy: t => Promise.t<unit>
  let make: (bool, method) => Promise.t<result<t, Js.Exn.t>>
}

module Client: Client = {
  type t = {
    mutable client: LanguageClient.t,
    queue: array<(Request.t, Response.t => unit)>,
    subscription: VSCode.Disposable.t,
  }

  // for emitting errors
  let errorChan: Chan.t<Js.Exn.t> = Chan.make()
  // for emitting data
  let dataChan: Chan.t<Js.Json.t> = Chan.make()

  let onError = callback => errorChan->Chan.on(callback)->VSCode.Disposable.make
  let onData = callback => dataChan->Chan.on(callback)->VSCode.Disposable.make

  let sendRequest = (self, data) =>
    self.client
    ->LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.flatMapOk(() => {
      self.client->LanguageClient.sendRequest("agda", data)->Promise.Js.toResult
    })

  let destroy = self => {
    self.subscription->VSCode.Disposable.dispose->ignore
    self.client->LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
  }

  let make = (devMode, method) => {
    // let emittedError = ref(false)

    let serverOptions =
      method == ViaTCP
        ? ServerOptions.makeWithStreamInfo(3000)
        : ServerOptions.makeWithCommand("als")

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

      let errorHandler: ErrorHandler.t = devMode
        ? ErrorHandler.make(
            ~error=(exn, _msg, _count) => {
              errorChan->Chan.emit(exn)
              Shutdown
            },
            ~closed=() => {
              DoNotRestart
            },
          )
        : ErrorHandler.makeDefault("Agda", 3)

      LanguageClientOptions.make(documentSelector, synchronize, errorHandler)
    }

    // Create the language client
    let languageClient = LanguageClient.make(
      "agdaLanguageServer",
      "Agda Language Server",
      serverOptions,
      clientOptions,
    )

    let self = {
      client: languageClient,
      queue: [],
      subscription: languageClient->LanguageClient.start,
    }

    // Let `LanguageClient.onReady` and `errorChan->Chan.once` race
    Promise.race(list{
      self.client->LanguageClient.onReady->Promise.Js.toResult,
      errorChan->Chan.once->Promise.map(err => Error(err)),
    })->Promise.map(result =>
      switch result {
      | Error(error) => Error(error)
      | Ok() =>
        // NOTE: somehow `onNotification` gets called TWICE everytime
        // This flag is for filtering out half of the Notifications
        let flag = ref(true)
        self.client->LanguageClient.onNotification("agda", json => {
          if flag.contents {
            dataChan->Chan.emit(json)
            flag := false
          } else {
            flag := true
          }
        })
        Ok(self)
      }
    )
  }
}

module Request = {
  type t = Initialize

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Initialize => object_(list{("tag", string("ReqInitialize"))})
    }
}

module Response = {
  type version = string
  type t =
    | Initialize(option<version>)
    // from client
    | NotConnectedYet
    | CannotEncodeRequest(string)
    | CannotDecodeResponse(string, Js.Json.t)
    // from server
    | CannotDecodeRequest(string)

  let fromJsError = (error: 'a): string => %raw("function (e) {return e.toString()}")(error)

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "ResInitialize" => Contents(optional(string) |> map(version => Initialize(version)))
    | "ResCannotDecodeRequest" => Contents(string |> map(version => CannotDecodeRequest(version)))
    | tag => raise(DecodeError("[LSP.Response] Unknown constructor: " ++ tag))
    }
  )
}

module type Module = {
  // methods
  let find: unit => Promise.t<result<string, Connection.Error.t>>

  let start: bool => Promise.t<option<Response.version>>
  let stop: unit => Promise.t<unit>
  let sendRequest: Request.t => Promise.t<Response.t>
  let changeMethod: method => Promise.t<option<Response.version>>
  let getVersion: unit => option<Response.version>
  // predicate
  let isConnected: unit => bool
  // output
  // let onResponse: (Response.t => unit) => VSCode.Disposable.t
  let onError: (Js.Exn.t => unit) => VSCode.Disposable.t
  let onChangeStatus: (status => unit) => VSCode.Disposable.t
  let onChangeMethod: (method => unit) => VSCode.Disposable.t
}

module Module: Module = {
  // for emitting events
  let statusChan: Chan.t<status> = Chan.make()
  let methodChan: Chan.t<method> = Chan.make()

  // for internal bookkeeping
  type state =
    | Disconnected
    | Connected(Client.t, Response.version)

  // internal states
  type singleton = {
    mutable state: state,
    mutable method: method,
    mutable devMode: bool,
  }
  let singleton: singleton = {
    state: Disconnected,
    method: ViaStdIO,
    devMode: false,
  }

  // locate the languege server
  let find = () => {
    Process.PathSearch.run("als")
    ->Promise.mapOk(Js.String.trim)
    ->Promise.mapError(e => Connection.Error.PathSearch(e))
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
      client->Client.destroy
    }

  // let decodeResponse = (json: Js.Json.t): Response.t =>
  //   switch // catching exceptions occured when decoding JSON values
  //   Response.decode(json) {
  //   | response => response
  //   | exception Json.Decode.DecodeError(msg) => CannotDecodeResponse(msg, json)
  //   }

  let sendRequestWithClient = (client, request) => {
    client
    ->Client.sendRequest(Request.encode(request))
    ->Promise.map(x =>
      switch x {
      | Ok(json) => Response.decode(json)
      | Error(error) =>
        statusChan->Chan.emit(Disconnected)
        Response.CannotEncodeRequest(Response.fromJsError(error))
      }
    )
  }

  // make and start the LSP client
  let rec startWithMethod = (devMode, method) => {
    // state
    switch singleton.state {
    | Disconnected =>
      Client.make(devMode, method)->Promise.flatMap(result =>
        switch result {
        | Error(exn) =>
          let isECONNREFUSED =
            Js.Exn.message(exn)->Option.mapWithDefault(
              false,
              Js.String.startsWith("connect ECONNREFUSED"),
            )
          let shouldSwitchToStdIO = isECONNREFUSED && method == ViaTCP

          if shouldSwitchToStdIO {
            Js.log("Connecting via TCP failed, switch to StdIO")
            singleton.method = ViaStdIO
            methodChan->Chan.emit(ViaStdIO)
            singleton.state = Disconnected
            statusChan->Chan.emit(Disconnected)
            startWithMethod(devMode, ViaStdIO)
          } else {
            singleton.state = Disconnected
            statusChan->Chan.emit(Disconnected)
            Promise.resolved(None)
          }
        | Ok(client) =>
          // send `ReqInitialize` and wait for `ResInitialize` before doing anything else
          sendRequestWithClient(client, Initialize)->Promise.flatMap(response =>
            switch response {
            | CannotDecodeResponse(msg, json) => Promise.resolved(None)
            | CannotEncodeRequest(msg) => Promise.resolved(None)
            | CannotDecodeRequest(msg) => Promise.resolved(None)
            | NotConnectedYet => Promise.resolved(None)
            // the server cannot find Agda
            | Initialize(None) => Promise.resolved(None)
            | Initialize(Some(version)) =>
              // update the status
              singleton.state = Connected(client, version)
              statusChan->Chan.emit(Connected)
              Promise.resolved(Some(version))
            }
          )
        }
      )
    | Connected(_, version) => Promise.resolved(Some(version))
    }
  }

  // make and start the LSP client
  let start = devMode => {
    singleton.devMode = devMode
    singleton.method = devMode ? ViaTCP : ViaStdIO
    startWithMethod(devMode, singleton.method)
  }

  let getVersion = () =>
    switch singleton.state {
    | Disconnected => None
    | Connected(_, version) => Some(version)
    }

  let isConnected = () =>
    switch singleton.state {
    | Disconnected => false
    | Connected(_, version) => true
    }

  // let onResponse = handler => Client.onData(json => handler(decodeResponse(json)))
  let onError = Client.onError
  let onChangeStatus = callback => statusChan->Chan.on(callback)->VSCode.Disposable.make
  let onChangeMethod = callback => methodChan->Chan.on(callback)->VSCode.Disposable.make

  let sendRequest = request =>
    switch singleton.state {
    | Connected(client, _version) => sendRequestWithClient(client, request)
    | Disconnected => Promise.resolved(Response.NotConnectedYet)
    }

  let changeMethod = method => {
    // update the state and reconfigure the connection
    if singleton.method != method {
      singleton.method = method
      methodChan->Chan.emit(method)
      stop()->Promise.flatMap(() => {
        start(singleton.devMode)
      })
    } else {
      Promise.resolved(None)
    }
  }
}

include Module
