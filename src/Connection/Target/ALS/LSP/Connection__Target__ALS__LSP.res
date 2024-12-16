module LSP = Connection__Target__ALS__LSP__Binding
module IPC = Connection__IPC

module type Module = {
  type t
  // lifecycle
  let make: (string, string, IPC.t, Js.Json.t) => promise<result<t, Js.Exn.t>>
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

let fromJsPromise = async (promise: promise<'t>): result<'t, Js.Exn.t> =>
  switch await promise {
  | result => Ok(result)
  | exception Js.Exn.Error(e) => Error(e)
  }

module Module: Module = {
  open VSCode

  type t = {
    client: LSP.LanguageClient.t,
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
      await self.client->LSP.LanguageClient.start
      await self.client->LSP.LanguageClient.sendNotification(self.id, data)
    })

  let sendRequest = (self, data) =>
    Util.Promise_.catch(async () => {
      await self.client->LSP.LanguageClient.start
      await self.client->LSP.LanguageClient.sendRequest(self.id, data)
    })

  let onRequest = (self, callback) =>
    self.client->LSP.LanguageClient.onRequest(self.id, callback)->LSP.Disposable.toVSCodeDisposable

  let destroy = self => {
    self.errorChan->Chan.destroy
    self.notificationChan->Chan.destroy
    LSP.LanguageClient.stop(self.client, Some(200))->fromJsPromise
  }

  let make = async (id, name, method, initializationOptions) => {
    let errorChan = Chan.make()

    let serverOptions = switch method {
    | IPC.ViaTCP(port, host, _) => LSP.ServerOptions.makeWithStreamInfo(port, host)
    | ViaPipe(path, args, options, _) => LSP.ServerOptions.makeWithCommand(path, args, options)
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

      let errorHandler: LSP.ErrorHandler.t = LSP.ErrorHandler.make(
        ~error=(exn, _msg, _count) => {
          errorChan->Chan.emit(exn)
          Shutdown
        },
        ~closed=() => {
          DoNotRestart
        },
      )

      LSP.LanguageClientOptions.make(
        documentSelector,
        synchronize,
        errorHandler,
        initializationOptions,
      )
    }

    // Create the language client
    let languageClient = LSP.LanguageClient.make(id, name, serverOptions, clientOptions)

    let self = {
      client: languageClient,
      id,
      name,
      method,
      errorChan,
      notificationChan: Chan.make(),
    }

    switch await self.client->LSP.LanguageClient.start->fromJsPromise {
    | Error(e) => Error(e)
    | Ok() => Ok(self)
    }
  }
}

include Module
