// Web-compatible fetch implementation
module Fetch = {
  type response
  type readableStreamReader
  type readResult = {done: bool, value: option<Uint8Array.t>}
  
  @val external fetch: (string, {"headers": {"User-Agent": string}}) => promise<response> = "fetch"
  @get external ok: response => bool = "ok"
  @get external status: response => int = "status"
  @get external headers: response => {"location": option<string>} = "headers"
  @get external body: response => readableStreamReader = "body"
  @send external getReader: readableStreamReader => readableStreamReader = "getReader"
  @send external read: readableStreamReader => promise<readResult> = "read"
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
      let response = await Fetch.fetch(url, options)
      if Fetch.ok(response) {
        Ok(response)
      } else {
        // Create a generic error for non-200 status codes
        let error = Obj.magic({"message": "HTTP " ++ Belt.Int.toString(Fetch.status(response))})
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
      let text = await %raw(`response.text()`)
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
          Some(%raw(`response.headers.get('content-length')`))
        } catch {
        | _ => None
        }
        let totalSize = switch contentLength {
        | Some(length) when length != %raw(`null`) => 
          Belt.Int.fromString(%raw(`length`))->Option.getOr(0)
        | _ => 0
        }
        
        // Get readable stream reader with null check
        let reader = try {
          if %raw(`response.body === null || response.body === undefined`) {
            raise(Failure("Response body is null or undefined"))
          }
          Ok(%raw(`response.body.getReader()`))
        } catch {
        | Failure(msg) => Error(msg)
        | Js.Exn.Error(exn) => Error(Js.Exn.message(exn)->Option.getOr("Failed to get response body reader"))
        | _ => Error("Unknown error getting response body reader")
        }
        
        switch reader {
        | Error(msg) => 
          let error = Obj.magic({"message": msg})
          Error(Error.CannotWriteFile(error))
        | Ok(reader) =>
          let chunks = ref([])
          let accumSize = ref(0)
          
          // Read chunks and track progress
          let rec readChunks = async () => {
            let result = await %raw(`reader.read()`)
            let done = %raw(`result.done`)
            let value = %raw(`result.value`)
            
            if !done {
              // Add chunk to accumulator
              chunks := Array.concat(chunks.contents, [value])
              
              // Update progress
              let chunkSize = %raw(`value.length`)
              accumSize := accumSize.contents + chunkSize
              onDownload(Event.Progress(accumSize.contents, totalSize))
              
              await readChunks()
            }
          }
          
          await readChunks()
          
          // Combine all chunks into one Uint8Array
          let totalLength = accumSize.contents
          let combinedArray = %raw(`new Uint8Array(totalLength)`)
          let offset = ref(0)
          
          chunks.contents->Array.forEach(chunk => {
            %raw(`combinedArray.set(chunk, offset.contents)`)
            let chunkLength = %raw(`chunk.length`)
            offset := offset.contents + chunkLength
          })
          
          // Write to file using FS module
          switch await FS.writeFile(destUri, combinedArray) {
          | Ok() =>
            onDownload(Event.Finish)
            Ok()
          | Error(error) => 
            let exn = Obj.magic({"message": error})
            Error(Error.CannotWriteFile(exn))
          }
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
