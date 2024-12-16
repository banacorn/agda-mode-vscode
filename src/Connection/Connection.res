module Error = Connection__Error
module Agda = Connection__Target__Agda
module ALS = Connection__Target__ALS
module Resolver = Connection__Resolver

module type Module = {
  type version = string
  type status =
    | Agda(version, string) // version of Agda, path of executable
    | ALS(version, Connection__IPC.t) // version of Agda, method of connection
  // lifecycle
  let start: (
    VSCode.Uri.t,
    bool,
    Resolver.GitHub.Download.Event.t => unit,
  ) => promise<result<status, Error.t>>
  let stop: unit => promise<result<unit, Error.t>>
  // messaging
  let sendRequest: (
    VSCode.Uri.t,
    Resolver.GitHub.Download.Event.t => unit,
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
  type connection = Agda(Agda.t) | ALS(ALS.t)
  let singleton: ref<option<connection>> = ref(None)

  type version = string
  type status =
    | Agda(version, string) // version of Agda, path of executable
    | ALS(version, Connection__IPC.t) // version of Agda, method of connection

  // connection -> status
  let toStatus = (conn: connection): status =>
    switch conn {
    | ALS(conn) => ALS(conn.version, ALS.getIPCMethod(conn))
    | Agda(conn) =>
      let (version, path) = Agda.getInfo(conn)
      Agda(version, path)
    }

  let start = async (globalStorageUri, useALS, onDownload) =>
    switch singleton.contents {
    | Some(conn) => Ok(toStatus(conn))
    | None =>
      if useALS {
        let (result, errors) = await Connection__Probe.probeLSP(
          VSCode.Uri.toString(globalStorageUri),
          onDownload,
        )
        switch result {
        | None => Error(Error.CannotAcquireHandle("Agda Language Server", errors))
        | Some(method) =>
          switch await ALS.make(method, InitOptions.getFromConfig()) {
          | Error(error) => Error(ALS(error))
          | Ok(conn) =>
            let method = ALS.getIPCMethod(conn)
            singleton := Some(ALS(conn))
            Ok(ALS(conn.version, method))
          }
        }
      } else {
        let (result, errors) = await Connection__Probe.probeEmacs()
        switch result {
        | None =>
          let name = Config.Connection.getAgdaVersion()
          Error(Error.CannotAcquireHandle(name, errors))
        | Some(method) =>
          switch await Agda.make(method) {
          | Error(error) => Error(Error.Agda(error))
          | Ok(conn) =>
            singleton := Some(Agda(conn))
            let (version, path) = Agda.getInfo(conn)
            Ok(Agda(version, path))
          }
        }
      }
    }

  let stop = async () =>
    switch singleton.contents {
    | None => Ok()
    | Some(Agda(conn)) =>
      singleton := None
      await Agda.destroy(conn)
      Ok()
    | Some(ALS(conn)) =>
      singleton := None
      switch await ALS.destroy(conn) {
      | Error(error) => Error(Error.ALS(error))
      | Ok(_) => Ok()
      }
    }

  let rec sendRequest = async (
    globalStorageUri,
    onDownload,
    useALS,
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
    | Some(ALS(conn)) =>
      let handler = x => x->Util.Result.mapError(err => Error.ALS(err))->handler
      switch await ALS.sendRequest(conn, encodeRequest(document, conn.version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await stop()
        Error(Error.ALS(error))
      | Ok(_) => Ok(toStatus(ALS(conn)))
      }

    | Some(Agda(conn)) =>
      let (version, _path) = Agda.getInfo(conn)
      let handler = x => x->Util.Result.mapError(err => Error.Agda(err))->handler
      switch await Agda.sendRequest(conn, encodeRequest(document, version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await stop()
        Error(Error.Agda(error))
      | Ok(_) => Ok(toStatus(Agda(conn)))
      }
    | None =>
      switch await start(globalStorageUri, useALS, onDownload) {
      | Error(error) => Error(error)
      | Ok(_) => await sendRequest(globalStorageUri, onDownload, useALS, document, request, handler)
      }
    }
  }
}

include Module
