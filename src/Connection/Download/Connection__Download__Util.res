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
    | Progress(int, int)
    | Finish

  let toString = event =>
    switch event {
    | Start => "Starting"
    | Progress(accum, total) =>
      // if the file is larger than 10MB than we use MB as the unit
      total > 10485760
        ? "" ++
          string_of_int(accum / 1048576) ++
          " MB / " ++
          string_of_int(total / 1048576) ++ " MB"
        : "" ++ string_of_int(accum / 1024) ++ " KB / " ++ string_of_int(total / 1024) ++ " MB"
    | Finish => "Done"
    }
}

module Progress = {
  let report = async name => {
    let progressRef = ref(None)

    // only resolves after the download progress hits 100% or the promise is rejected
    let (promise, resolve, _) = Util.Promise_.pending()

    // for keeping track of the download progress
    let percentage = ref(0)

    event =>
      switch event {
      | Event.Start =>
        // instantiate the progress bar and steal the progress report function
        VSCode.Window.withProgress(
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
      | Event.Progress(accum, total) =>
        let percentageNew = int_of_float(float_of_int(accum) /. float_of_int(total) *. 100.0)

        let increment = percentageNew - percentage.contents
        percentage := percentageNew

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
      }
  }
}

module Module: {
  let asJson: {"headers": {"User-Agent": string}, "host": string, "path": string} => promise<
    result<Js.Json.t, Error.t>,
  >
  let asFile: (
    {"headers": {"User-Agent": string}, "host": string, "path": string},
    VSCode.Uri.t,
    Event.t => unit,
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
    let fetchOptions = {"headers": httpOptions["headers"]}
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

  let asFile = async (httpOptions, destUri, onDownload) => {
    let url = optionsToUrl(httpOptions)
    let fetchOptions = {"headers": httpOptions["headers"]}
    switch await fetchWithRedirects(url, fetchOptions) {
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
        | Some(Some(length)) => Belt.Int.fromString(length)->Option.getOr(0)
        | _ => 0
        }
        
        // Use arrayBuffer instead of streaming for simplicity with rescript-fetch
        let arrayBuffer = await arrayBuffer(response)
        let uint8Array = Core__Uint8Array.fromBuffer(arrayBuffer)
        
        // Report progress
        let fileSize = Core__TypedArray.length(uint8Array)
        onDownload(Event.Progress(fileSize, totalSize))
        
        // Write to file using FS module
        switch await FS.writeFile(destUri, uint8Array) {
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
