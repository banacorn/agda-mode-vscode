module Error = Connection__Error
module Scheduler = Connection__Scheduler
module Emacs = Connection__Emacs
module LSP = Connection__LSP

module type Module = {
  type version = string
  type status = Emacs(version, string) | LSP(version, LSP.method)
  // lifecycle
  let start: (bool, bool) => Promise.t<result<status, Error.t>>
  let stop: unit => Promise.t<unit>
  // messaging
  let sendRequest: (
    bool,
    bool,
    VSCode.TextDocument.t,
    Request.t,
    result<Response.t, Error.t> => Promise.t<unit>,
  ) => Promise.t<result<status, Error.t>>
}

module Module: Module = {
  // internal state singleton
  type connection = Emacs(Emacs.t) | LSP(LSP.t)
  let singleton: ref<option<connection>> = ref(None)

  type version = string
  type status = Emacs(version, string) | LSP(version, LSP.method)

  // connection -> status
  let toStatus = (conn: connection): status =>
    switch conn {
    | LSP(conn) =>
      let (version, method) = LSP.getStatus(conn)
      LSP(version, method)
    | Emacs(conn) =>
      let (version, path) = Emacs.getStatus(conn)
      Emacs(version, path)
    }

  let start = (useLSP, viaTCP) =>
    switch singleton.contents {
    | Some(conn) => Promise.resolved(Ok(toStatus(conn)))
    | None =>
      if useLSP {
        LSP.make(viaTCP)->Promise.mapOk(conn => {
          let (version, method) = LSP.getStatus(conn)
          singleton := Some(LSP(conn))
          LSP(version, method)
        })
      } else {
        Emacs.make()->Promise.mapOk(conn => {
          singleton := Some(Emacs(conn))
          let (version, path) = Emacs.getStatus(conn)
          Emacs(version, path)
        })
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
      let (version, _method) = LSP.getStatus(conn)
      LSP.sendRequest(conn, encodeRequest(document, version), handler)->Promise.mapOk(() =>
        toStatus(LSP(conn))
      )
    | Some(Emacs(conn)) =>
      let (version, _path) = Emacs.getStatus(conn)
      Emacs.sendRequest(conn, encodeRequest(document, version), handler)->Promise.mapOk(() =>
        toStatus(Emacs(conn))
      )
    | None =>
      start(useLSP, viaTCP)->Promise.flatMapOk(_ =>
        sendRequest(useLSP, viaTCP, document, request, handler)
      )
    }
  }
}

include Module
