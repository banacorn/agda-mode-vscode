module Scheduler = Connection__Scheduler
module Error = Connection__LSP__Error
module LSP = LanguageServerMule.LSP

type method = ViaStdIO(string, string) | ViaTCP(int)
type version = string

module type Module = {
  type t
  // lifecycle
  let make: method => Promise.t<result<t, Error.t>>
  let destroy: t => Promise.t<unit>
  // input / output / event
  let sendRequest: (t, Js.Json.t) => Promise.t<result<Js.Json.t, Error.t>>
  let onResponse: (Js.Json.t => unit) => VSCode.Disposable.t
  let onError: (Error.t => unit) => VSCode.Disposable.t
  // properties
  let getMethod: t => method
}

module Module: Module = {
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
    errorChan->Chan.on(e => callback(Error.Connection(e)))->VSCode.Disposable.make
  let onResponse = callback => dataChan->Chan.on(callback)->VSCode.Disposable.make

  let sendRequest = (self, data) =>
    self.client
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.flatMapOk(() => {
      self.client->LSP.LanguageClient.sendRequest("agda", data)->Promise.Js.toResult
    })
    ->Promise.mapError(exn => Error.Connection(exn))

  let destroy = self => {
    self.subscription->VSCode.Disposable.dispose->ignore
    self.client->LSP.LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
  }

  let make = method => {
    let serverOptions = switch method {
    | ViaTCP(port) => LSP.ServerOptions.makeWithStreamInfo(port, "localhost")
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
    })->Promise.map(result =>
      switch result {
      | Error(error) => Error(Error.Connection(error))
      | Ok() =>
        self.client->LSP.LanguageClient.onRequest("agda", json => {
          dataChan->Chan.emit(json)
          Promise.resolved()
        })
        Ok(self)
      }
    )
  }

  let getMethod = conn => conn.method
}

include Module
