module Scheduler = Connection__Scheduler
module Error = Connection__LSP__Error
module Binding = Connection__LSP__Binding

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
    client: Binding.LanguageClient.t,
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
    ->Binding.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.flatMapOk(() => {
      self.client->Binding.LanguageClient.sendRequest("agda", data)->Promise.Js.toResult
    })
    ->Promise.mapError(exn => Error.Connection(exn))

  let destroy = self => {
    self.subscription->VSCode.Disposable.dispose->ignore
    self.client->Binding.LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
  }

  let make = method => {
    let serverOptions = switch method {
    | ViaTCP(port) => Binding.ServerOptions.makeWithStreamInfo(port)
    | ViaStdIO(name, _path) => Binding.ServerOptions.makeWithCommand(name)
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

      let errorHandler: Binding.ErrorHandler.t = Binding.ErrorHandler.make(
        ~error=(exn, _msg, _count) => {
          errorChan->Chan.emit(exn)
          Shutdown
        },
        ~closed=() => {
          DoNotRestart
        },
      )
      Binding.LanguageClientOptions.make(documentSelector, synchronize, errorHandler)
    }

    // Create the language client
    let languageClient = Binding.LanguageClient.make(
      "agdaLanguageServer",
      "Agda Language Server",
      serverOptions,
      clientOptions,
    )

    let self = {
      client: languageClient,
      subscription: languageClient->Binding.LanguageClient.start,
      method: method,
    }

    // Let `LanguageClient.onReady` and `errorChan->Chan.once` race
    Promise.race(list{
      self.client->Binding.LanguageClient.onReady->Promise.Js.toResult,
      errorChan->Chan.once->Promise.map(err => Error(err)),
    })->Promise.map(result =>
      switch result {
      | Error(error) => Error(Error.Connection(error))
      | Ok() =>
        self.client->Binding.LanguageClient.onRequest("agda", json => {
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
