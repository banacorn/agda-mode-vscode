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
        let increment = switch totalOpt {
        | Some(total) =>
          let percentageNew = int_of_float(float_of_int(accum) /. float_of_int(total) *. 100.0)
          let inc = percentageNew - percentage.contents
          percentage := percentageNew
          inc
        | None => 0
        }

        // only report progress if the increment > 0
        if increment > 0 {
          progressRef.contents->Option.forEach(progress =>
            progress->VSCode.Progress.report({
              "increment": increment,
              "message": Event.toString(event),
            })
          )
        }
      | Event.Finish =>
        progressRef.contents->Option.forEach(progress =>
          progress->VSCode.Progress.report({"increment": 100, "message": Event.toString(event)})
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
    ~trace: Connection__Download__Trace.t => unit=?,
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
  let fetchWithRedirects = async (url, options) => {
    try {
      let response = await Fetch.fetch(url, {
        method: #GET,
        headers: Fetch.Headers.make(Fetch.Headers.Init.object(options)),
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
    ~trace as _trace=Connection__Download__Trace.noop,
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
    switch await fetch(url, fetchOptions) {
    | Ok(response) =>
      onDownload(Event.Start)

      try {
        // Get content length for progress tracking
        let contentLength = try {
          Some(Fetch.Response.headers(response)->Fetch.Headers.get("content-length"))
        } catch {
        | _ => None
        }
        let totalSize = switch contentLength {
        | Some(Some(length)) => Belt.Int.fromString(length)
        | _ => None
        }

        let uint8Array = await readBody(response)

        // Report progress
        let fileSize = Core__TypedArray.length(uint8Array)
        onDownload(Event.Progress(fileSize, totalSize))

        // Write to file using FS module
        switch await writeFile(destUri, uint8Array) {
        | Ok() =>
          onDownload(Event.Finish)
          Ok()
        | Error(error) =>
          let exn = Obj.magic({"message": error})
          Error(Error.CannotWriteFile(exn))
        }
      } catch {
      | Js.Exn.Error(obj) => Error(Error.CannotWriteFile(obj))
      | _ =>
        let error = Obj.magic({"message": "Failed to download file"})
        Error(Error.CannotWriteFile(error))
      }
    | Error(e) => Error(e)
    }
  }
}
include Module
