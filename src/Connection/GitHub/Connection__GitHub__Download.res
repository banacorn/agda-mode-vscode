module Https = {
  @module("https")
  external get: (
    {"host": string, "path": string, "headers": {"User-Agent": string}},
    NodeJs.Http.IncomingMessage.t => unit,
  ) => unit = "get"

  @module("https")
  external getWithUrl: (string, NodeJs.Http.IncomingMessage.t => unit) => unit = "get"
}

module Error = {
  type t =
    | ServerResponseError(Js.Exn.t)
    | NoRedirectLocation
    | Timeout(int) // ms
    | JsonParseError(string)
    | CannotWriteFile(Js.Exn.t)

  let toString = x =>
    switch x {
    | ServerResponseError(exn) => "Server response error:\n" ++ Util.JsError.toString(exn)
    | NoRedirectLocation => "Got HTTP 301/302 from GitHub without location in headers"
    | Timeout(time) =>
      "Timeout after " ++ string_of_int(time) ++ "ms. Please check your internet connection"
    | JsonParseError(raw) => "Cannot parse downloaded file as JSON:\n" ++ raw
    | CannotWriteFile(exn) =>
      "Failed to write downloaded content to files:\n" ++ Util.JsError.toString(exn)
    }
}

module Event = {
  type t =
    | Start
    | Progress(int, int)
    | Finish

  let toString = event =>
    switch event {
    | Start => "Start downloading"
    | Progress(accum, total) =>
      // if the file is larger than 10MB than we use MB as the unit
      total > 10485760
        ? "Downloading ( " ++
          string_of_int(accum / 1048576) ++
          " MB / " ++
          string_of_int(total / 1048576) ++ " MB )"
        : "Downloading ( " ++
          string_of_int(accum / 1024) ++
          " KB / " ++
          string_of_int(total / 1024) ++ " MB )"
    | Finish => "Finish downloading"
    }
}

module Module: {
  let asJson: {"headers": {"User-Agent": string}, "host": string, "path": string} => promise<
    result<Js.Json.t, Error.t>,
  >
  let asFile: (
    {"headers": {"User-Agent": string}, "host": string, "path": string},
    string,
    Event.t => unit,
  ) => promise<result<unit, Error.t>>

  let timeoutAfter: (promise<result<'a, Error.t>>, int) => promise<result<'a, Error.t>>
} = {
  let gatherDataFromResponseStream = res => {
    open NodeJs.Http.IncomingMessage
    let body = ref("")
    Promise.make((resolve, _) => {
      res->onData(buf => body := body.contents ++ NodeJs.Buffer.toString(buf))->ignore
      res->onError(error => resolve(Error(Error.ServerResponseError(error))))->ignore
      res->onClose(() => resolve(Ok(body.contents)))->ignore
    })
  }

  // with HTTP 301/302 redirect
  let getWithRedirects = options => {
    Promise.make((resolve, _) => {
      Https.get(options, res => {
        // check the response status code first
        let statusCode = NodeJs.Http.IncomingMessage.statusCode(res)
        switch statusCode {
        // redirect
        | 301
        | 302 =>
          let headers = NodeJs.Http.IncomingMessage.headers(res)
          switch headers.location {
          | None => resolve(Error(Error.NoRedirectLocation))
          | Some(urlAfterRedirect) =>
            Https.getWithUrl(urlAfterRedirect, resAfterRedirect => resolve(Ok(resAfterRedirect)))
          }
        // ok ?
        | _ => resolve(Ok(res))
        }
      })
    })
  }

  // helper combinator for timeout
  let timeoutAfter = (p, n) => {
    Promise.race([
      Promise.make((resolve, _) => {
        Js.Global.setTimeout(() => resolve(Error(Error.Timeout(n))), n)->ignore
      }),
      p,
    ])
  }

  let asJson = async httpOptions =>
    switch await getWithRedirects(httpOptions) {
    | Ok(res) =>
      switch await gatherDataFromResponseStream(res) {
      | Ok(raw) =>
        try {
          Ok(Js.Json.parseExn(raw))
        } catch {
        | _ => Error(Error.JsonParseError(raw))
        }
      | Error(e) => Error(e)
      }
    | Error(e) => Error(e)
    }

  let asFile = async (httpOptions, destPath, onDownload) =>
    switch await getWithRedirects(httpOptions) {
    | Ok(res) =>
      onDownload(Event.Start)
      // calculate and report download progress
      let totalSize =
        NodeJs.Http.IncomingMessage.headers(res).contentLenth->Option.mapOr(
          0,
          int_of_string,
        )
      let accumSize = ref(0)
      res
      ->NodeJs.Http.IncomingMessage.onData(chunk => {
        let chunkSize = NodeJs.Buffer.length(chunk)
        accumSize := accumSize.contents + chunkSize
        onDownload(Event.Progress(accumSize.contents, totalSize))
      })
      ->ignore

      // pipe the response to a file
      await Promise.make((resolve, _) => {
        let fileStream = NodeJs.Fs.createWriteStream(destPath)
        fileStream
        ->NodeJs.Fs.WriteStream.onError(exn => resolve(Error(Error.CannotWriteFile(exn))))
        ->ignore
        fileStream
        ->NodeJs.Fs.WriteStream.onClose(() => {
          // report Event.Finish
          onDownload(Finish)
          // resolve the promise
          resolve(Ok())
        })
        ->ignore
        res->NodeJs.Http.IncomingMessage.pipe(fileStream)->ignore
      })
    | Error(e) => Error(e)
    }
}
include Module
