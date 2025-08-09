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
    Js.Json.t,
  ) => t = %raw("function (documentSelector, synchronize, errorHandler, initializationOptions) {
      return {
		    documentSelector: documentSelector,
		    synchronize: synchronize,
        errorHandler: errorHandler,
        initializationOptions: initializationOptions
      }
    }")
}

type executableOptions = {
    cwd?: string,
    env?: Js.Dict.t<string>,
    detached?: bool,
    shell?: bool
}

// Options to control the language client
module ServerOptions = {
  type t
  let makeWithCommand: (
    string,
    array<string>,
    option<executableOptions>,
  ) => t = %raw("function (command, args, options) {
      return { 
        command: command, 
        args: args, 
        options: options
       }
    }")

  let makeWithStreamInfo: (int, string) => t = %raw("function (port, host) {
      const net = require('net');
      const socket = net.createConnection({ port: port, host: host })
      return (() => { return new Promise(resolve => resolve({
        writer: socket,
        reader: socket
      })
      )})
    }")
}

// module WebviewEditorInset = {
//   type t
//   // properties
//   @get external editor: t => VSCode.TextEditor.t = "editor"
//   @get external line: t => int = "line"
//   @get external height: t => int = "height"
//   @get external webview: t => VSCode.Webview.t = "webview"
//   @get external onDidDispose: t => VSCode.Event.t<unit> = "onDidDispose"
//   // methods
//   @send external dispose: t => unit = "dispose"
// }

// module WindowExt = {
//   @module("vscode") @scope("window")
//   external createWebviewTextEditorInset: (VSCode.TextEditor.t, int, int) => WebviewEditorInset.t =
//     "createWebviewTextEditorInset"
//   @module("vscode") @scope("window")
//   external createWebviewTextEditorInsetWithOptions: (
//     VSCode.TextEditor.t,
//     int,
//     int,
//     VSCode.WebviewOptions.t,
//   ) => WebviewEditorInset.t = "createWebviewTextEditorInset"
// }

module Disposable = {
  type t
  // methods
  @send external dispose: t => unit = "dispose"

  let toVSCodeDisposable = self => VSCode.Disposable.make(() => dispose(self))
}
module LanguageClient = {
  type t
  // constructor
  @module("vscode-languageclient") @new
  external make: (string, string, ServerOptions.t, LanguageClientOptions.t) => t = "LanguageClient"
  // methods
  @send external start: t => promise<unit> = "start"

  // default wait time: 2 seconds
  @send external stop: (t, option<int>) => promise<unit> = "stop"
  @send
  external onNotification: (t, string, 'a) => Disposable.t = "onNotification"
  // https://github.com/microsoft/vscode-languageserver-node/blob/02806427ce7251ec8fa2ff068febd9a9e59dbd2f/client/src/common/client.ts#L811C68-L811C81
  @send
  external sendNotification: (t, string, 'a) => promise<unit> = "sendNotification"
  @send
  external sendRequest: (t, string, Js.Json.t) => promise<'result> = "sendRequest"
  @send
  external onRequest: (t, string, 'a => promise<'result>) => Disposable.t = "onRequest"
}
