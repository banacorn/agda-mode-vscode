module Error = Connection__Error
module Scheduler = Connection__Scheduler
module Emacs = Connection__Emacs
module LSP = Connection__LSP

module type Module = {
  type version = string
  type status =
    | Emacs(version, string) // version of Agda, path of executable
    | LSP(version, LanguageServerMule.Method.t) // version of Agda, method of connection
  // lifecycle
  let start: (
    string,
    bool,
    LanguageServerMule.Source.GitHub.Download.Event.t => unit,
  ) => Promise.t<result<status, Error.t>>
  let stop: unit => Promise.t<result<unit, Error.t>>
  // messaging
  let sendRequest: (
    string,
    LanguageServerMule.Source.GitHub.Download.Event.t => unit,
    bool,
    VSCode.TextDocument.t,
    Request.t,
    result<Response.t, Error.t> => Promise.t<unit>,
  ) => Promise.t<result<status, Error.t>>
}

module Module: Module = {
  module InitOptions = {
    type t = {commandLineOptions: array<string>}

    let encode = ({commandLineOptions}) => {
      open JsonCombinators.Json.Encode
      Unsafe.object({
        "commandLineOptions": array(string, commandLineOptions),
      })
    }

    let getFromConfig = () =>
      {
        commandLineOptions: Config.Connection.getCommandLineOptions(),
      }->encode
  }

  // internal state singleton
  type connection = Emacs(Emacs.t) | LSP(LSP.t)
  let singleton: ref<option<connection>> = ref(None)

  type version = string
  type status =
    | Emacs(version, string) // version of Agda, path of executable
    | LSP(version, LanguageServerMule.Method.t) // version of Agda, method of connection

  // connection -> status
  let toStatus = (conn: connection): status =>
    switch conn {
    | LSP(conn) => LSP(conn.version, LanguageServerMule.Client.LSP.getMethod(conn.client))
    | Emacs(conn) =>
      let (version, path) = Emacs.getInfo(conn)
      Emacs(version, path)
    }

  let start = (globalStoragePath, useLSP, onDownload) =>
    switch singleton.contents {
    | Some(conn) => Promise.resolved(Ok(toStatus(conn)))
    | None =>
      if useLSP {
        Connection__Probe.probeLSP(globalStoragePath, onDownload)
        ->Promise.flatMap(((result, errors)) =>
          switch result {
          | None =>
            Promise.resolved(Error(Error.CannotAcquireHandle("Agda Language Server", errors)))
          | Some(method) => Promise.resolved(Ok(method))
          }
        )
        ->Promise.flatMapOk(method => {
          LSP.Client.make("agda", "Agda Language Server", method, InitOptions.getFromConfig())
          ->Util.P.toPromise
          ->Promise.mapError(e => LSP.Error.ConnectionError(e))
          ->Promise.flatMapOk(LSP.make)
          ->Promise.mapError(error => Error.LSP(error))
        })
        ->Promise.mapOk(conn => {
          let method = LanguageServerMule.Client.LSP.getMethod(conn.client)
          singleton := Some(LSP(conn))
          LSP(conn.version, method)
        })
      } else {
        Connection__Probe.probeEmacs()
        ->Promise.flatMap(((result, errors)) =>
          switch result {
          | None =>
            let name = Config.Connection.getAgdaVersion()
            Promise.resolved(Error(Error.CannotAcquireHandle(name, errors)))
          | Some(method) => Promise.resolved(Ok(method))
          }
        )
        ->Promise.flatMapOk(method =>
          Emacs.make(method)->Promise.mapError(error => Error.Emacs(error))
        )
        ->Promise.mapOk(conn => {
          singleton := Some(Emacs(conn))
          let (version, path) = Emacs.getInfo(conn)
          Emacs(version, path)
        })
      }
    }

  let stop = () =>
    switch singleton.contents {
    | None => Promise.resolved(Ok())
    | Some(Emacs(conn)) =>
      singleton := None
      Emacs.destroy(conn)->Promise.map(() => Ok())
    | Some(LSP(conn)) =>
      singleton := None
      LSP.destroy(conn)->Promise.mapError(err => Error.LSP(err))
    }

  let rec sendRequest = (globalStoragePath, onDownload, useLSP, document, request, handler) => {
    // encode the Request to some string
    let encodeRequest = (document, version) => {
      let filepath = document->VSCode.TextDocument.fileName->Parser.filepath
      let libraryPath = Config.getLibraryPath()
      let highlightingMethod = Config.Highlighting.getHighlightingMethod()
      let backend = Config.getBackend()
      Request.encode(document, version, filepath, backend, libraryPath, highlightingMethod, request)
    }

    switch singleton.contents {
    | Some(LSP(conn)) =>
      let handler = x => x->Util.Result.mapError(err => Error.LSP(err))->handler
      LSP.sendRequest(conn, encodeRequest(document, conn.version), handler)
      ->Promise.mapOk(() => toStatus(LSP(conn)))
      ->Promise.flatMapError(error => {
        // stop the connection on error
        stop()->Promise.map(_ => Error(Error.LSP(error)))
      })

    | Some(Emacs(conn)) =>
      let (version, _path) = Emacs.getInfo(conn)
      let handler = x => x->Util.Result.mapError(err => Error.Emacs(err))->handler
      Emacs.sendRequest(conn, encodeRequest(document, version), handler)
      ->Promise.mapOk(() => toStatus(Emacs(conn)))
      ->Promise.flatMapError(error => {
        // stop the connection on error
        stop()
        // ->Promise.mapError(err => Error.LSP(err))
        ->Promise.map(_ => Error(Error.Emacs(error)))
      })
    | None =>
      start(globalStoragePath, useLSP, onDownload)->Promise.flatMapOk(_ =>
        sendRequest(globalStoragePath, onDownload, useLSP, document, request, handler)
      )
    }
  }
}

include Module
