module Binding = Connection__Protocol__LSP__Binding
module IPC = Connection__Transport

module type Module = {
  type t
  // lifecycle
  let make: (
    string,
    string,
    IPC.t,
    option<Binding.executableOptions>,
    Js.Json.t,
  ) => promise<result<t, Js.Exn.t>>
  let destroy: t => promise<result<unit, Js.Exn.t>>
  // request / notification / error
  let sendRequest: (t, Js.Json.t) => promise<result<Js.Json.t, Js.Exn.t>>
  let onRequest: (t, Js.Json.t => promise<result<Js.Json.t, Js.Exn.t>>) => VSCode.Disposable.t
  let sendNotification: (t, Js.Json.t) => promise<result<unit, Js.Exn.t>>
  let onNotification: (t, Js.Json.t => unit) => VSCode.Disposable.t
  let onError: (t, Js.Exn.t => unit) => VSCode.Disposable.t
  // channels for notification & error
  let getNotificationChan: t => Chan.t<Js.Json.t>
  let getErrorChan: t => Chan.t<Js.Exn.t>
}

let fromJsPromise = async (promise: promise<'t>): result<'t, Js.Exn.t> => {
  switch await promise {
  | result => Ok(result)
  | exception Js.Exn.Error(e) => Error(e)
  | exception e =>
    // Convert any exception to Js.Exn.t using standard JS pattern
    // In JS: error instanceof Error ? error : new Error(String(error))
    let jsError: Js.Exn.t = %raw("
      (function(e) {
        if (e instanceof Error) {
          return e;
        } else {
          return new Error(String(e));
        }
      })
    ")(e)
    Error(jsError)
  }
}

module Module: Module = {
  open VSCode

  type t = {
    client: Binding.LanguageClient.t,
    id: string, // language id, also for identifying custom methods
    name: string, // name for the language server client
    method: IPC.t,
    // event emitters
    errorChan: Chan.t<Js.Exn.t>,
    notificationChan: Chan.t<Js.Json.t>,
  }

  let onError = (self, callback) =>
    self.errorChan->Chan.on(e => callback(e))->VSCode.Disposable.make

  let getErrorChan = self => self.errorChan

  let onNotification = (self, callback) =>
    self.notificationChan->Chan.on(callback)->VSCode.Disposable.make

  let getNotificationChan = self => self.notificationChan

  let sendNotification = (self, data) =>
    Util.Promise_.catch(async () => {
      await self.client->Binding.LanguageClient.start
      await self.client->Binding.LanguageClient.sendNotification(self.id, data)
    })

  let sendRequest = (self, data) =>
    Util.Promise_.catch(async () => {
      await self.client->Binding.LanguageClient.start
      await self.client->Binding.LanguageClient.sendRequest(self.id, data)
    })

  let onRequest = (self, callback) =>
    self.client
    ->Binding.LanguageClient.onRequest(self.id, callback)
    ->Binding.Disposable.toVSCodeDisposable

  let destroy = self => {
    self.errorChan->Chan.destroy
    self.notificationChan->Chan.destroy
    Binding.LanguageClient.stop(self.client, Some(200))->fromJsPromise
  }

  let make = async (id, name, method, serverInitOptions, clientInitOptions) => {
    let errorChan = Chan.make()

    let serverOptions = switch method {
    | IPC.ViaTCP(_, url) => Binding.ServerOptions.makeWithStreamInfo(url.port, url.hostname)
    | ViaPipe(path, args) => Binding.ServerOptions.makeWithCommand(path, args, serverInitOptions)
    }

    let clientOptions = {
      // Register the server for plain text documents
      let documentSelector: DocumentSelector.t = [
        StringOr.make(
          Others({
            open DocumentFilter
            {
              scheme: Some("file"),
              pattern: None,
              language: Some(id),
            }
          }),
        ),
      ]

      // Notify the server about file changes to '.clientrc files contained in the workspace
      let synchronize: FileSystemWatcher.t = Workspace.createFileSystemWatcher(
        %raw("'**/.clientrc'"),
        ~ignoreCreateEvents=false,
        ~ignoreChangeEvents=false,
        ~ignoreDeleteEvents=false,
      )

      let errorHandler: Binding.ErrorHandler.t = Binding.ErrorHandler.make(
        ~error=(exn, _msg, _count) => {
          errorChan->Chan.emit(exn)
          Shutdown
        },
        ~closed=() => {
          DoNotRestart
        },
      )

      Binding.LanguageClientOptions.make(
        documentSelector,
        synchronize,
        errorHandler,
        clientInitOptions,
      )
    }

    // Create the language client
    let languageClient = Binding.LanguageClient.make(id, name, serverOptions, clientOptions)

    let self = {
      client: languageClient,
      id,
      name,
      method,
      errorChan,
      notificationChan: Chan.make(),
    }

    switch await self.client->Binding.LanguageClient.start->fromJsPromise {
    | Error(e) => Error(e)
    | Ok() => Ok(self)
    }
  }
}

include Module
