module Error = Connection__Error
module Scheduler = Connection__Scheduler
module Emacs = Connection__Emacs
module LSP = Connection__LSP

module type Module = {
  type version = string
  type status = Emacs(version) | LSP(LSP.method, version)
  // lifecycle
  let start: (bool, bool) => Promise.t<result<status, Connection__Error.t>>
  let reconnect: unit => Promise.t<unit>
  let stop: unit => Promise.t<unit>
  // messaging
  let sendRequest: (
    bool,
    bool,
    VSCode.TextDocument.t,
    Request.t,
    result<Response.t, Connection__Error.t> => Promise.t<unit>,
  ) => Promise.t<result<unit, Error.t>>
}

module Module: Module = {
  // internal state singleton
  type connection = Emacs(Emacs.t) | LSP(LSP.t)
  let singleton: ref<option<connection>> = ref(None)

  type version = string
  type status = Emacs(version) | LSP(LSP.method, version)

  let start = (useLSP, viaTCP) =>
    switch singleton.contents {
    | Some(LSP(conn)) =>
      let (method, version) = LSP.getStatus(conn)
      Promise.resolved(Ok(LSP(method, version)))
    | Some(Emacs(conn)) =>
      let version = Emacs.getVersion(conn)
      Promise.resolved(Ok(Emacs(version)))
    | None =>
      if useLSP {
        LSP.make(viaTCP)->Promise.map(result =>
          switch result {
          | Ok(conn) =>
            let (method, version) = LSP.getStatus(conn)
            singleton := Some(LSP(conn))
            Ok(LSP(method, version))
          | Error(error) => Error(Error.LSP(error))
          }
        )
      } else {
        Emacs.make()->Promise.map(result =>
          switch result {
          | Ok(conn) =>
            singleton := Some(Emacs(conn))
            let version = Emacs.getVersion(conn)
            Ok(Emacs(version))
          | Error(error) => Error(error)
          }
        )
      }
    }

  let stop = () =>
    switch singleton.contents {
    | Some(LSP(conn)) =>
      singleton := None
      LSP.destroy(conn)
    | Some(Emacs(conn)) =>
      singleton := None
      Emacs.destroy(conn)
    | None => Promise.resolved()
    }

  let rec sendRequest = (useLSP, viaTCP, document, request, handler) => {
    // encode the Request to some string
    let encodeRequest = (document, version) => {
      let filepath = document->VSCode.TextDocument.fileName->Parser.filepath
      let libraryPath = Config.getLibraryPath()
      let highlightingMethod = Config.getHighlightingMethod()
      let backend = Config.getBackend()
      Request.encode(document, version, filepath, backend, libraryPath, highlightingMethod, request)
    }

    switch singleton.contents {
    | Some(LSP(conn)) =>
      let handlerLSP = result =>
        switch result {
        | Error(error) => handler(Error(Error.LSP(error)))
        | Ok(response) => handler(Ok(response))
        }
      let (_method, version) = LSP.getStatus(conn)
      LSP.sendRequest(conn, encodeRequest(document, version), handlerLSP)->Promise.map(result =>
        switch result {
        | Ok() => Ok()
        | Error(error) => Error(Error.LSP(error))
        }
      )
    | Some(Emacs(conn)) =>
      let version = Emacs.getVersion(conn)
      Emacs.sendRequest(conn, encodeRequest(document, version), handler)
    | None =>
      start(useLSP, viaTCP)->Promise.flatMapOk(_ =>
        sendRequest(useLSP, viaTCP, document, request, handler)
      )
    }
  }

  let reconnect = () => Promise.resolved()
}

include Module
