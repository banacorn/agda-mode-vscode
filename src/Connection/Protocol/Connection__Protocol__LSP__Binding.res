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
      error,
      closed,
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

  // Variant that includes uriConverters (needed for WASM/worker setups)
  let makeWithUriConverters: (
    DocumentSelector.t,
    FileSystemWatcher.t,
    ErrorHandler.t,
    Js.Json.t,
    'uriConverters,
  ) => t = %raw("function (documentSelector, synchronize, errorHandler, initializationOptions, uriConverters) {
      return {
        documentSelector: documentSelector,
        synchronize: synchronize,
        errorHandler: errorHandler,
        initializationOptions: initializationOptions,
        uriConverters: uriConverters
      }
    }")

  let setConfigurationMiddleware: t => unit = %raw("function (options) {
      if (!options || typeof options !== 'object') {
        return;
      }

      const safeArray = (value) => Array.isArray(value) ? value.filter((item) => typeof item === 'string') : [];

      const readCommandLineOptions = () => {
        try {
          const vscode = require('vscode');
          const config = vscode.workspace.getConfiguration('agdaMode');
          const raw = config.get('connection.commandLineOptions');
          if (typeof raw === 'string') {
            return raw.trim().length === 0 ? [] : raw.trim().split(/\s+/);
          }
          if (Array.isArray(raw)) {
            return safeArray(raw);
          }
        } catch (_) {
          // ignore and fall back to empty array
        }
        return [];
      };

      const createPayload = () => ({ commandLineOptions: readCommandLineOptions() });

      options.middleware = options.middleware || {};
      options.middleware.workspace = options.middleware.workspace || {};
      options.middleware.workspace.configuration = (params) => {
        const count = params && Array.isArray(params.items) ? params.items.length : 1;
        const results = [];
        for (let i = 0; i < count; i += 1) {
          results.push(createPayload());
        }
        return Promise.resolve(results);
      };
    }")
}

type executableOptions = {
  cwd?: string,
  env?: Js.Dict.t<string>,
  detached?: bool,
  shell?: bool,
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

  let makeWithWASM: WASMLoader.t => t = %raw(`
      (wasmSetup) => {
      const createTransport = () => {
        const attemptWithSetup = () => wasmSetup.factory.createServer(
          wasmSetup.memfsAgdaDataDir,
          {},
          {
            presetupCallback: wasmSetup.presetupCallback,
            runSetupFirst: true,
            setupCallback(exitCode, stderr) {
              if (exitCode !== 0) {
                console.warn('[agda-mode] ALS WASM setup exited with code ' + exitCode + ': ' + stderr);
              }
            },
          },
        );

        const attemptWithoutSetup = () => wasmSetup.factory.createServer(
          wasmSetup.memfsAgdaDataDir,
          {},
          {
            presetupCallback: wasmSetup.presetupCallback,
            runSetupFirst: false
          },
        );

        return Promise.resolve()
          .then(attemptWithSetup)
          .catch((error) => {
            const message = error && (error.message || String(error));
            if (typeof message === 'string' && message.includes('--setup')) {
              return attemptWithoutSetup();
            }
            throw error;
          });
      };

        Object.defineProperty(createTransport, "__agdaModeWasm", {
          value: true,
          enumerable: false,
          configurable: false,
          writable: false,
        });

        return createTransport;
      }
    `)
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
  // constructor helper
  @module("vscode-languageclient")
  external languageClientConstructor: 'a = "LanguageClient"

  let makeShim = (_languageClientCtor) =>
    %raw(`
    (id, name, serverOptions, clientOptions) => {
      // for retaining the reference to the transport when server options is resolved
      let transport;
      let onTransportReady;
      const waitUntilTransportSet = new Promise(resolve => {
        onTransportReady = resolve;
      });

      // serverOptions can be a string to a command (native), or a function to a promise resolving to transports (WASM)
      const shouldDisposeServer = typeof serverOptions === 'function';
      const serverThunk = shouldDisposeServer ?
        () => serverOptions().then(t => {
          transport = t;
          onTransportReady();
          return t;
        }) :
        serverOptions;

      let ctor = _languageClientCtor;
      if (ctor && typeof ctor !== "function" && ctor.default && typeof ctor.default === "function") {
        ctor = ctor.default;
      }
      if (typeof ctor !== "function") {
        throw new Error("agda-mode: unable to locate vscode-languageclient constructor");
      }

      let client;
      const ctorArity = typeof ctor.length === "number" ? ctor.length : 0;
      if (ctorArity === 4) {
        // before language client v10, we need a shim in order to use it on the web
        if (typeof serverOptions !== "function" || !serverOptions.__agdaModeWasm) {
          throw new Error("agda-mode: browser LanguageClient requires WASM transport");
        }
        // see https://github.com/microsoft/vscode-languageserver-node/blob/release/client/8.1.0/client/src/browser/main.ts
        client = new ctor(id, name, clientOptions, serverThunk);
        if (client.worker !== serverThunk) {
          throw new Error("agda-mode: failed to shim the browser version of language client; remove it if using client v10");
        }
        client.createMessageTransports = () => serverThunk();
      } else {
        // the 5-arg constructor is the desktop one
        client = new ctor(id, name, serverThunk, clientOptions);
      }

      if (shouldDisposeServer) {
        const origDispose = client.dispose.bind(client);
        client.dispose = async timeout => {
          if (client._disposed) return;
          // _disposed should be set by calling this
          await origDispose(timeout);
          // call dispose on the transport, which should in turn terminate the worker
          let ret = -1;
          if (transport == null) {
            await waitUntilTransportSet;
          }
          // older ALS WASM loader might not support this
          if (transport.dispose) {
            ret = await transport.dispose();
            transport = undefined;
          }
          return ret;
        }
      }

      return client;
    }
  `)

  let make: (string, string, ServerOptions.t, LanguageClientOptions.t) => t = makeShim(
    languageClientConstructor,
  )

  // methods
  @send external start: t => promise<unit> = "start"

  // default wait time: 2 seconds
  @send external stop: (t, option<int>) => promise<unit> = "stop"

  @send external dispose: t => promise<'a> = "dispose"

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
