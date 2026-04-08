// Web-compatible fetch implementation using rescript-fetch

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

@send external arrayBuffer: Fetch.Response.t => promise<ArrayBuffer.t> = "arrayBuffer"

module Event = {
  type t =
    | Start
    | Progress(int, option<int>)
    | Finish
    | Fail

  let toString = event =>
    switch event {
    | Start => "Starting"
    | Progress(accum, Some(total)) =>
      // if the file is larger than 10MB than we use MB as the unit
      total > 10485760
        ? "" ++
          string_of_int(accum / 1048576) ++
          " MB / " ++
          string_of_int(total / 1048576) ++ " MB"
        : "" ++ string_of_int(accum / 1024) ++ " KB / " ++ string_of_int(total / 1024) ++ " MB"
    | Progress(accum, None) =>
      string_of_int(accum / 1024) ++ " KB"
    | Finish => "Done"
    | Fail => "Failed"
    }
}

module Progress = {
  let report = async (name, ~withProgress=VSCode.Window.withProgress) => {
    let progressRef = ref(None)

    // only resolves after the download progress hits 100% or the promise is rejected
    let (promise, resolve, _) = Util.Promise_.pending()

    // for keeping track of the download progress
    let percentage = ref(0)

    let handler: Event.t => unit = event =>
      switch event {
      | Event.Start =>
        // instantiate the progress bar and steal the progress report function
        withProgress(
          {
            location: VSCode.ProgressLocation.Notification,
            title: "Downloading " ++ name,
          },
          (progress, _cancellationToken) => {
            progress->VSCode.Progress.report({"increment": 0, "message": Event.toString(event)}) // set the progress bar to 0%
            progressRef := Some(progress) // steal the progress report function
            promise // return a promise that resolves after the download progress hits 100%
          },
        )->ignore
      | Event.Progress(accum, totalOpt) =>
        switch totalOpt {
        | Some(total) when total > 0 =>
          let percentageNew = min(100, int_of_float(float_of_int(accum) /. float_of_int(total) *. 100.0))
          let inc = percentageNew - percentage.contents
          percentage := percentageNew
          if inc > 0 {
            progressRef.contents->Option.forEach(progress =>
              progress->VSCode.Progress.report({
                "increment": inc,
                "message": Event.toString(event),
              })
            )
          }
        | Some(_)
        | None =>
          progressRef.contents->Option.forEach(progress =>
            progress->VSCode.Progress.report({"increment": 0, "message": Event.toString(event)})
          )
        }
      | Event.Finish =>
        let remaining = max(0, 100 - percentage.contents)
        progressRef.contents->Option.forEach(progress =>
          progress->VSCode.Progress.report({"increment": remaining, "message": Event.toString(event)})
        )
        resolve()
      | Event.Fail =>
        progressRef.contents->Option.forEach(progress =>
          progress->VSCode.Progress.report({"increment": 0, "message": Event.toString(event)})
        )
        resolve()
      }

    handler
  }
}

module Module: {
  let asJson: {"headers": {"User-Agent": string}, "host": string, "path": string} => promise<
    result<Js.Json.t, Error.t>,
  >
  let asFile: (
    {"headers": {..}, "host": string, "path": string},
    VSCode.Uri.t,
    Event.t => unit,
    ~trace: Connection__Download__Trace.callback=?,
    ~fetch: (string, {"headers": {..}}) => promise<result<Fetch.Response.t, Error.t>>=?,
    ~readBody: Fetch.Response.t => promise<Core__Uint8Array.t>=?,
    ~writeFile: (VSCode.Uri.t, Core__Uint8Array.t) => promise<result<unit, string>>=?,
  ) => promise<result<unit, Error.t>>

  let timeoutAfter: (promise<result<'a, Error.t>>, int) => promise<result<'a, Error.t>>
} = {
  // Convert http options to URL
  let optionsToUrl = options => 
    "https://" ++ options["host"] ++ options["path"]

  // Web-compatible fetch with redirects (fetch handles redirects automatically)
  let fetchWithRedirects = async (url, options: {"headers": {..}}) => {
    try {
      let response = await Fetch.fetch(url, {
        method: #GET,
        headers: Fetch.Headers.make(Fetch.Headers.Init.object(options["headers"])),
      })
      if Fetch.Response.ok(response) {
        Ok(response)
      } else {
        // Create a generic error for non-200 status codes
        let error = Obj.magic({"message": "HTTP " ++ Belt.Int.toString(Fetch.Response.status(response))})
        Error(Error.ServerResponseError(error))
      }
    } catch {
    | Js.Exn.Error(obj) => Error(Error.ServerResponseError(obj))
    | _ => 
      let error = Obj.magic({"message": "Network error"})
      Error(Error.ServerResponseError(error))
    }
  }

  // Read response body as text for JSON
  let gatherDataFromResponse = async response => {
    try {
      let text = await Fetch.Response.text(response)
      Ok(text)
    } catch {
    | Js.Exn.Error(obj) => Error(Error.ServerResponseError(obj))
    | _ => 
      let error = Obj.magic({"message": "Failed to read response body"})
      Error(Error.ServerResponseError(error))
    }
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

  let asJson = async httpOptions => {
    let url = optionsToUrl(httpOptions)
    // Detect environment: Node.js has 'process' global, browsers have 'window'
    let fetchOptions: {..} = %raw(`typeof process !== 'undefined' && process.versions && process.versions.node`) 
      ? {"headers": httpOptions["headers"]} // Desktop (Node.js) - keep User-Agent
      : %raw(`{}`) // Web (browser) - no custom headers to avoid CORS preflight
    switch await fetchWithRedirects(url, fetchOptions) {
    | Ok(response) =>
      switch await gatherDataFromResponse(response) {
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
  }

  let defaultReadBody = async (response: Fetch.Response.t) => {
    let ab = await arrayBuffer(response)
    Core__Uint8Array.fromBuffer(ab)
  }

  let asFile = async (
    httpOptions: {"headers": {..}, "host": string, "path": string},
    destUri,
    onDownload,
    ~trace=Connection__Download__Trace.noop,
    ~fetch=fetchWithRedirects,
    ~readBody=defaultReadBody,
    ~writeFile=FS.writeFile,
  ) => {
    let url = optionsToUrl(httpOptions)
    // Detect environment: Node.js has 'process' global, browsers have 'window'
    // On web, Accept header is CORS-safe but User-Agent triggers preflight
    let headers = %raw(`
      (typeof process !== 'undefined' && process.versions && process.versions.node)
        ? httpOptions.headers
        : {"Accept": httpOptions.headers["Accept"]}
    `)
    let fetchOptions = {"headers": headers}
    trace(Connection__Download__Trace.FetchStarted(url))
    onDownload(Event.Start)
    switch await fetch(url, fetchOptions) {
    | Error(e) =>
      trace(Connection__Download__Trace.Failed("fetch", Error.toString(e)))
      onDownload(Event.Fail)
      Error(e)
    | Ok(response) =>
      let status = Fetch.Response.status(response)
      let contentLength = try {
        Some(Fetch.Response.headers(response)->Fetch.Headers.get("content-length"))
      } catch {
      | _ => None
      }
      let totalSize = switch contentLength {
      | Some(Some(length)) => Belt.Int.fromString(length)
      | _ => None
      }
      trace(Connection__Download__Trace.FetchResponseReceived(status, totalSize))

      try {
        trace(Connection__Download__Trace.BodyReadStarted)
        let uint8Array = await readBody(response)
        let fileSize = Core__TypedArray.length(uint8Array)
        trace(Connection__Download__Trace.BodyProgress(fileSize, totalSize))
        trace(Connection__Download__Trace.BodyReadCompleted(fileSize))
        onDownload(Event.Progress(fileSize, totalSize))

        trace(Connection__Download__Trace.WriteStarted(destUri))
        switch await writeFile(destUri, uint8Array) {
        | Ok() =>
          trace(Connection__Download__Trace.WriteCompleted(destUri))
          onDownload(Event.Finish)
          Ok()
        | Error(error) =>
          trace(Connection__Download__Trace.Failed("write", error))
          onDownload(Event.Fail)
          let exn = Obj.magic({"message": error})
          Error(Error.CannotWriteFile(exn))
        }
      } catch {
      | Js.Exn.Error(obj) =>
        let msg = Util.JsError.toString(obj)
        trace(Connection__Download__Trace.Failed("body", msg))
        onDownload(Event.Fail)
        Error(Error.CannotWriteFile(obj))
      | _ =>
        let msg = "Failed to download file"
        trace(Connection__Download__Trace.Failed("body", msg))
        onDownload(Event.Fail)
        let error = Obj.magic({"message": msg})
        Error(Error.CannotWriteFile(error))
      }
    }
  }
}
include Module
