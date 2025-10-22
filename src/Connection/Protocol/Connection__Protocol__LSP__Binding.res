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
            runSetupFirst: true,
            setupCallback(exitCode, stderr) {
              if (exitCode === 0) {
                console.log('[agda-mode] ALS WASM setup completed successfully.');
              } else {
                console.warn('[agda-mode] ALS WASM setup exited with code ' + exitCode + ': ' + stderr);
              }
            },
          },
        );

        const attemptWithoutSetup = () => wasmSetup.factory.createServer(
          wasmSetup.memfsAgdaDataDir,
          {},
          { runSetupFirst: false },
        );

        return Promise.resolve()
          .then(attemptWithSetup)
          .catch((error) => {
            const message = error && (error.message || String(error));
            if (typeof message === 'string' && message.includes('--setup')) {
              console.warn('[agda-mode] ALS WASM binary does not support --setup; retrying without setup.');
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

  let createStreamBackedWorker = %raw(`(factory) => {
        const messageListeners = new Set();
        const errorListeners = new Set();
        let onmessageHandler = null;
        let settledTransport = null;
        let disposables = [];

        const emitMessage = (data) => {
          console.log('[agda-mode] worker emitMessage', JSON.stringify(data, null, 2));
          const event = { data };
          if (typeof onmessageHandler === "function") {
            onmessageHandler(event);
          }
          messageListeners.forEach((listener) => listener(event));
        };

        const emitError = (error) => {
          console.error('[agda-mode] worker emitError', error instanceof Error ? error.stack : JSON.stringify(error, null, 2));
          const event = { type: "error", error };
          errorListeners.forEach((listener) => listener(event));
        };

        const ensureTransport = Promise.resolve()
          .then(() => (typeof factory === "function" ? factory() : factory))
          .then((transport) => {
            console.log('[agda-mode] worker transport ready', JSON.stringify(transport, null, 2));
            if (!transport || typeof transport !== "object") {
              throw new Error("agda-mode: invalid WASM transport");
            }
            const reader = transport.reader;
            const writer = transport.writer;
            if (!reader || typeof reader.listen !== "function") {
              throw new Error("agda-mode: WASM transport missing reader");
            }
            if (!writer || typeof writer.write !== "function") {
              throw new Error("agda-mode: WASM transport missing writer");
            }
            settledTransport = transport;
            const listenerDisposable = reader.listen((data) => {
              console.log('[agda-mode] reader received data', JSON.stringify(data, null, 2));
              emitMessage(data);
            });
            if (listenerDisposable && typeof listenerDisposable.dispose === "function") {
              disposables.push(listenerDisposable);
            }
            if (typeof reader.onError === "function") {
              const errorDisposable = reader.onError((arg) => emitError(Array.isArray(arg) ? arg[0] : arg));
              if (errorDisposable && typeof errorDisposable.dispose === "function") {
                disposables.push(errorDisposable);
              }
            }
            if (typeof reader.onClose === "function") {
              const closeDisposable = reader.onClose(() => emitError(new Error("Language server stream closed")));
              if (closeDisposable && typeof closeDisposable.dispose === "function") {
                disposables.push(closeDisposable);
              }
            }
            return transport;
          });

        ensureTransport.catch((error) => {
          emitError(error);
          throw error;
        });

        const worker = {};
        console.log('[agda-mode] worker created');
        Object.defineProperty(worker, "onmessage", {
          get() {
            return onmessageHandler;
          },
          set(handler) {
            onmessageHandler = typeof handler === "function" ? handler : null;
          },
          enumerable: true,
          configurable: true,
        });

        worker.addEventListener = (type, listener) => {
          if (type === "message") {
            messageListeners.add(listener);
          } else if (type === "error") {
            errorListeners.add(listener);
          }
        };

        worker.removeEventListener = (type, listener) => {
          if (type === "message") {
            messageListeners.delete(listener);
          } else if (type === "error") {
            errorListeners.delete(listener);
          }
        };

        worker.postMessage = (message) => {
          console.log('[agda-mode] worker postMessage', JSON.stringify(message, null, 2));
          ensureTransport
            .then((transport) => {
              console.log('[agda-mode] worker writing to transport');
              transport.writer.write(message).catch(emitError);
            })
            .catch(() => {});
        };

        worker.terminate = () => {
          console.log('[agda-mode] worker terminate');
          disposables.forEach((disposable) => {
            try {
              if (disposable && typeof disposable.dispose === "function") {
                disposable.dispose();
              }
            } catch (_) {}
          });
          disposables = [];
          if (settledTransport && settledTransport.writer && typeof settledTransport.writer.end === "function") {
            try {
              settledTransport.writer.end();
            } catch (_) {}
          }
          settledTransport = null;
        };

        return worker;
      }
  `)

  let makeShim = (_languageClientCtor, _createStreamBackedWorker) =>
    %raw(`
    (id, name, serverOptions, clientOptions) => {

      let ctor = _languageClientCtor;
      let createStreamBackedWorker = _createStreamBackedWorker;
      if (ctor && typeof ctor !== "function" && ctor.default && typeof ctor.default === "function") {
        ctor = ctor.default;
      }
      if (typeof ctor !== "function") {
        throw new Error("agda-mode: unable to locate vscode-languageclient constructor");
      }

      const ctorArity = typeof ctor.length === "number" ? ctor.length : 0;
      if (ctorArity === 4) {
        if (typeof serverOptions !== "function" || !serverOptions.__agdaModeWasm) {
          throw new Error("agda-mode: browser LanguageClient requires WASM transport");
        }
        const worker = createStreamBackedWorker(serverOptions);
        return new ctor(id, name, clientOptions, worker);
      }

      return new ctor(id, name, serverOptions, clientOptions);
    }
  `)

  let make: (string, string, ServerOptions.t, LanguageClientOptions.t) => t = makeShim(
    languageClientConstructor,
    createStreamBackedWorker,
  )

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
