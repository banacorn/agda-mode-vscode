

module Error = Connection__Error
module Scheduler = Connection__Scheduler
module Emacs = Connection__Target__Agda
module ALS = Connection__Target__ALS
module LSP = LanguageServerMule.Client.LSP

module type Module = {
  type version = string
  type status =
    | Emacs(version, string) // version of Agda, path of executable
    | LSP(version, LanguageServerMule.Method.t) // version of Agda, method of connection
  // lifecycle
  let start: (
    VSCode.Uri.t,
    bool,
    LanguageServerMule.Source.GitHub.Download.Event.t => unit,
  ) => promise<result<status, Error.t>>
  let stop: unit => promise<result<unit, Error.t>>
  // messaging
  let sendRequest: (
    VSCode.Uri.t,
    LanguageServerMule.Source.GitHub.Download.Event.t => unit,
    bool,
    VSCode.TextDocument.t,
    Request.t,
    result<Response.t, Error.t> => promise<unit>,
  ) => promise<result<status, Error.t>>
}

module Module: Module = {
  module InitOptions = {
    type t = {commandLineOptions: array<string>}

    let encode = ({commandLineOptions}) => {
      open JsonCombinators.Json.Encode
      Unsafe.object({
        "commandLineOptions": array(string)(commandLineOptions),
      })
    }

    let getFromConfig = () =>
      {
        commandLineOptions: Config.Connection.getCommandLineOptions(),
      }->encode
  }

  // internal state singleton
  type connection = Emacs(Emacs.t) | LSP(ALS.t)
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

  let start = async (globalStorageUri, useLSP, onDownload) =>
    switch singleton.contents {
    | Some(conn) => Ok(toStatus(conn))
    | None =>
      if useLSP {
        let (result, errors) = await Connection__Probe.probeLSP(VSCode.Uri.toString(globalStorageUri), onDownload)
        switch result {
        | None => Error(Error.CannotAcquireHandle("Agda Language Server", errors))
        | Some(method) =>
          switch await LSP.make(
            "agda",
            "Agda Language Server",
            method,
            InitOptions.getFromConfig(),
          ) {
          | Error(error) => Error(LSP(ALS.Error.ConnectionError(error)))
          | exception Exn.Error(error) => Error(LSP(ALS.Error.ConnectionError(error)))
          | Ok(conn) =>
            switch await ALS.make(conn) {
            | Error(error) => Error(LSP(error))
            | Ok(conn) =>
              let method = LanguageServerMule.Client.LSP.getMethod(conn.client)
              singleton := Some(LSP(conn))
              Ok(LSP(conn.version, method))
            }
          }
        }
      } else {
        let (result, errors) = await Connection__Probe.probeEmacs()
        switch result {
        | None =>
          let name = Config.Connection.getAgdaVersion()
          Error(Error.CannotAcquireHandle(name, errors))
        | Some(method) =>
          switch await Emacs.make(method) {
          | Error(error) => Error(Error.Emacs(error))
          | Ok(conn) =>
            singleton := Some(Emacs(conn))
            let (version, path) = Emacs.getInfo(conn)
            Ok(Emacs(version, path))
          }
        }
      }
    }

  let stop = async () =>
    switch singleton.contents {
    | None => Ok()
    | Some(Emacs(conn)) =>
      singleton := None
      await Emacs.destroy(conn)
      Ok()
    | Some(LSP(conn)) =>
      singleton := None
      switch await ALS.destroy(conn) {
      | Error(error) => Error(Error.LSP(error))
      | Ok(_) => Ok()
      }
    }

  let rec sendRequest = async (
    globalStorageUri,
    onDownload,
    useLSP,
    document,
    request,
    handler,
  ) => {
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
      switch await ALS.sendRequest(conn, encodeRequest(document, conn.version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await stop()
        Error(Error.LSP(error))
      | Ok(_) => Ok(toStatus(LSP(conn)))
      }

    | Some(Emacs(conn)) =>
      let (version, _path) = Emacs.getInfo(conn)
      let handler = x => x->Util.Result.mapError(err => Error.Emacs(err))->handler
      switch await Emacs.sendRequest(conn, encodeRequest(document, version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await stop()
        Error(Error.Emacs(error))
      | Ok(_) => Ok(toStatus(Emacs(conn)))
      }
    | None =>
      switch await start(globalStorageUri, useLSP, onDownload) {
      | Error(error) => Error(error)
      | Ok(_) =>
        await sendRequest(globalStorageUri, onDownload, useLSP, document, request, handler)
      }
    }
  }
}

include Module
