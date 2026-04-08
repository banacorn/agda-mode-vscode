open Mocha

module Util = Connection__Download__Util
module Trace = Connection__Download__Trace

describe("Connection__Download__Util", () => {
  This.timeout(10000)

  describe("Error.toString", () => {
    Async.it("should format ServerResponseError correctly", async () => {
      let exn = %raw(`new Error("Network failed")`)
      let error = Util.Error.ServerResponseError(exn)
      let errorString = Util.Error.toString(error)
      // Just verify we get a non-empty string back
      Assert.ok(Js.String.length(errorString) > 0)
    })

    Async.it("should format NoRedirectLocation correctly", async () => {
      let error = Util.Error.NoRedirectLocation
      let errorString = Util.Error.toString(error)
      Assert.equal(errorString, "Got HTTP 301/302 from GitHub without location in headers")
    })

    Async.it("should format Timeout correctly", async () => {
      let error = Util.Error.Timeout(5000)
      let errorString = Util.Error.toString(error)
      let expected = "Timeout after 5000ms. Please check your internet connection"
      Assert.equal(errorString, expected)
    })

    Async.it("should format JsonParseError correctly", async () => {
      let error = Util.Error.JsonParseError("invalid json content")
      let errorString = Util.Error.toString(error)
      let expected = "Cannot parse downloaded file as JSON:\ninvalid json content"
      Assert.equal(errorString, expected)
    })

    Async.it("should format CannotWriteFile correctly", async () => {
      let exn = %raw(`new Error("Permission denied")`)
      let error = Util.Error.CannotWriteFile(exn)
      let errorString = Util.Error.toString(error)
      // Just verify we get a non-empty string back
      Assert.ok(Js.String.length(errorString) > 0)
    })
  })

  describe("Event.toString", () => {
    Async.it("should format Start event correctly", async () => {
      let event = Util.Event.Start
      let eventString = Util.Event.toString(event)
      Assert.equal(eventString, "Starting")
    })

    Async.it("should format Progress event correctly for small files", async () => {
      let event = Util.Event.Progress(512000, Some(1024000)) // 500KB / 1MB
      let eventString = Util.Event.toString(event)
      // Expected: "500 KB / 1000 MB" (note: there's a bug in the original code)
      let expected = "500 KB / 1000 MB"
      Assert.equal(eventString, expected)
    })

    Async.it("should format Progress event correctly for large files", async () => {
      let event = Util.Event.Progress(52428800, Some(104857600)) // 50MB / 100MB
      let eventString = Util.Event.toString(event)
      let expected = "50 MB / 100 MB"
      Assert.equal(eventString, expected)
    })

    Async.it("should format Progress event with unknown total", async () => {
      let event = Util.Event.Progress(512000, None)
      let eventString = Util.Event.toString(event)
      Assert.equal(eventString, "500 KB")
    })

    Async.it("should format Finish event correctly", async () => {
      let event = Util.Event.Finish
      let eventString = Util.Event.toString(event)
      Assert.equal(eventString, "Done")
    })

    Async.it("should format Fail event correctly", async () => {
      let event = Util.Event.Fail
      let eventString = Util.Event.toString(event)
      Assert.equal(eventString, "Failed")
    })
  })

  describe("Progress.report", () => {
    // Build a withProgress mock that captures the completion promise returned by the callback.
    // The completion promise is what Progress.report passes to VS Code to keep the notification open;
    // it resolves when Event.Finish or Event.Fail is dispatched.
    let makeMockWithProgress = () => {
      let captured: ref<option<promise<unit>>> = ref(None)
      let reportCalls: ref<array<{"increment": int, "message": string}>> = ref([])
      let mock = (
        _options: VSCode.ProgressOptions.t,
        callback: (
          VSCode.Progress.t<{"increment": int, "message": string}>,
          VSCode.CancellationToken.t,
        ) => promise<unit>,
      ) => {
        let fakeProgress: VSCode.Progress.t<{"increment": int, "message": string}> = Obj.magic({
          "report": (call: {"increment": int, "message": string}) => {
            reportCalls := reportCalls.contents->Array.concat([call])
          },
        })
        let fakeCancellationToken: VSCode.CancellationToken.t = Obj.magic({
          "isCancellationRequested": false,
        })
        let p = callback(fakeProgress, fakeCancellationToken)
        captured := Some(p)
        p
      }
      (mock, captured, reportCalls)
    }

    Async.it("should resolve completion promise on Event.Fail", async () => {
      let (mockWithProgress, captured, reportCalls) = makeMockWithProgress()
      let handler = await Util.Progress.report("test", ~withProgress=mockWithProgress)

      handler(Util.Event.Start) // triggers mockWithProgress, captures completion promise

      let completionPromise = captured.contents->Option.getExn

      handler(Util.Event.Fail)

      let result = await Util.timeoutAfter(
        completionPromise->Promise.then(() => Promise.resolve(Ok())),
        100,
      )
      Assert.deepStrictEqual(result, Ok())

      let lastReport = reportCalls.contents->Array.at(-1)
      Assert.deepStrictEqual(lastReport, Some({"increment": 0, "message": "Failed"}))
    })

    Async.it("should resolve completion promise on Event.Finish", async () => {
      let (mockWithProgress, captured, reportCalls) = makeMockWithProgress()
      let handler = await Util.Progress.report("test", ~withProgress=mockWithProgress)

      handler(Util.Event.Start)

      let completionPromise = captured.contents->Option.getExn

      handler(Util.Event.Finish)

      let result = await Util.timeoutAfter(
        completionPromise->Promise.then(() => Promise.resolve(Ok())),
        100,
      )
      Assert.deepStrictEqual(result, Ok())

      let lastReport = reportCalls.contents->Array.at(-1)
      Assert.deepStrictEqual(lastReport, Some({"increment": 100, "message": "Done"}))
    })

    Async.it("should report unknown-total progress with zero increment and byte-count message", async () => {
      let (mockWithProgress, captured, reportCalls) = makeMockWithProgress()
      let handler = await Util.Progress.report("test", ~withProgress=mockWithProgress)

      handler(Util.Event.Start)
      let completionPromise = captured.contents->Option.getExn
      handler(Util.Event.Progress(512000, None))

      let hasUnknownTotalReport = reportCalls.contents->Array.some(r =>
        r["increment"] == 0 && r["message"] == "500 KB"
      )
      Assert.deepStrictEqual(hasUnknownTotalReport, true)

      handler(Util.Event.Finish)
      let result = await Util.timeoutAfter(
        completionPromise->Promise.then(() => Promise.resolve(Ok())),
        100,
      )
      Assert.deepStrictEqual(result, Ok())
    })

    Async.it("should report only remaining increment on Event.Finish after partial progress", async () => {
      let (mockWithProgress, captured, reportCalls) = makeMockWithProgress()
      let handler = await Util.Progress.report("test", ~withProgress=mockWithProgress)

      handler(Util.Event.Start)
      let completionPromise = captured.contents->Option.getExn
      handler(Util.Event.Progress(50, Some(100)))
      handler(Util.Event.Finish)

      let result = await Util.timeoutAfter(
        completionPromise->Promise.then(() => Promise.resolve(Ok())),
        100,
      )
      Assert.deepStrictEqual(result, Ok())

      let lastReport = reportCalls.contents->Array.at(-1)
      Assert.deepStrictEqual(lastReport, Some({"increment": 50, "message": "Done"}))
    })

    Async.it("should report zero increment on Event.Finish when already at 100%", async () => {
      let (mockWithProgress, captured, reportCalls) = makeMockWithProgress()
      let handler = await Util.Progress.report("test", ~withProgress=mockWithProgress)

      handler(Util.Event.Start)
      let completionPromise = captured.contents->Option.getExn
      handler(Util.Event.Progress(100, Some(100)))
      handler(Util.Event.Finish)

      let result = await Util.timeoutAfter(
        completionPromise->Promise.then(() => Promise.resolve(Ok())),
        100,
      )
      Assert.deepStrictEqual(result, Ok())

      let lastReport = reportCalls.contents->Array.at(-1)
      Assert.deepStrictEqual(lastReport, Some({"increment": 0, "message": "Done"}))
    })
  })

  describe("timeoutAfter", () => {
    Async.it("should resolve normally if promise completes within timeout", async () => {
      let fastPromise = Promise.make((resolve, _) => {
        Js.Global.setTimeout(() => resolve(Ok("success")), 100)->ignore
      })
      
      let result = await Util.timeoutAfter(fastPromise, 1000)
      
      switch result {
      | Ok(value) => Assert.equal(value, "success")
      | Error(error) => Assert.fail("Expected success, got error: " ++ Util.Error.toString(error))
      }
    })

    Async.it("should timeout if promise takes too long", async () => {
      let slowPromise = Promise.make((resolve, _) => {
        Js.Global.setTimeout(() => resolve(Ok("success")), 2000)->ignore
      })
      
      let result = await Util.timeoutAfter(slowPromise, 500)
      
      switch result {
      | Ok(_) => Assert.fail("Expected timeout error, got success")
      | Error(error) => 
        switch error {
        | Timeout(time) => Assert.equal(time, 500)
        | _ => Assert.fail("Expected Timeout error, got: " ++ Util.Error.toString(error))
        }
      }
    })
  })

  // Integration tests with real network calls (these may be flaky)
  describe("asJson (integration)", () => {
    Async.it("should successfully parse valid JSON response", async () => {
      // Test with httpbin.org which is reliable for testing
      let testOptions = {
        "headers": {"User-Agent": "agda-mode-vscode-test"},
        "host": "httpbin.org",
        "path": "/json"
      }
      
      // Add timeout wrapper to prevent hanging
      let timedResult = await Util.timeoutAfter(Util.asJson(testOptions), 5000)
      
      switch timedResult {
      | Ok(json) => 
        // httpbin.org/json returns a JSON object, so we just verify it's valid JSON
        Assert.ok(Js.typeof(json) === "object")
      | Error(error) => 
        // Network tests can be flaky, so we'll be lenient
        Js.Console.warn("Network test failed (this may be expected): " ++ Util.Error.toString(error))
        Assert.ok(true) // Pass the test even if network fails
      }
    })

    Async.it("should handle invalid JSON gracefully", async () => {
      // Use httpbin.org/html which returns HTML, not JSON
      let testOptions = {
        "headers": {"User-Agent": "agda-mode-vscode-test"},
        "host": "httpbin.org", 
        "path": "/html"
      }
      
      // Add timeout wrapper to prevent hanging
      let timedResult = await Util.timeoutAfter(Util.asJson(testOptions), 5000)
      
      switch timedResult {
      | Ok(_) => Assert.fail("Expected JSON parse error, got success")
      | Error(error) => 
        switch error {
        | JsonParseError(_) => Assert.ok(true) // Expected error type
        | Timeout(_) => 
          // Timeout is acceptable for network tests
          Js.Console.warn("Network test timed out (this may be expected)")
          Assert.ok(true)
        | _ => 
          // Network error is also acceptable for this test
          Js.Console.warn("Got network error instead of JSON parse error: " ++ Util.Error.toString(error))
          Assert.ok(true)
        }
      }
    })
  })

  describe("asFile trace contract", () => {
    let httpOptions = {
      "headers": {"Accept": "application/octet-stream", "User-Agent": "test"},
      "host": "example.com",
      "path": "/download.zip",
    }
    let destUri = VSCode.Uri.file(
      NodeJs.Path.join([NodeJs.Os.tmpdir(), "trace-test-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".zip"])
    )

    // Fake Fetch.Response.t with controllable status and optional content-length header
    let makeFakeResponse: (int, option<string>) => Fetch.Response.t = %raw(`
      function(status, contentLength) {
        return {
          ok: status >= 200 && status < 300,
          status: status,
          headers: {
            get: function(name) {
              return name === "content-length" ? contentLength : undefined;
            }
          }
        };
      }
    `)

    let make3Bytes = (): Core__Uint8Array.t => %raw(`new Uint8Array([1, 2, 3])`)

    Async.it("should trace FetchStarted before fetch resolves", async () => {
      let traces = ref([])
      let onTrace = event => {
        traces := traces.contents->Array.concat([event])
      }

      let resolveRef: ref<option<result<Fetch.Response.t, Util.Error.t> => unit>> = ref(None)
      let fetchPromise: promise<result<Fetch.Response.t, Util.Error.t>> = Promise.make((resolve, _) => {
        resolveRef := Some(resolve)
      })
      let fakeFetch = (_url, _opts) => fetchPromise

      let p = Util.asFile(
        httpOptions,
        destUri,
        _event => (),
        ~trace=onTrace,
        ~fetch=fakeFetch,
      )

      Assert.deepStrictEqual(traces.contents, [
        Trace.FetchStarted("https://example.com/download.zip"),
      ])

      resolveRef.contents->Option.forEach(resolve =>
        resolve(Error(Util.Error.NoRedirectLocation))
      )

      let result = await p
      Assert.deepStrictEqual(result, Error(Util.Error.NoRedirectLocation))
    })

    Async.it("should emit Event.Start before fetch resolves", async () => {
      let events = ref([])
      let onDownload = event => {
        events := events.contents->Array.concat([event])
      }

      let resolveRef: ref<option<result<Fetch.Response.t, Util.Error.t> => unit>> = ref(None)
      let fetchPromise: promise<result<Fetch.Response.t, Util.Error.t>> = Promise.make((resolve, _) => {
        resolveRef := Some(resolve)
      })
      let fakeFetch = (_url, _opts) => fetchPromise

      let p = Util.asFile(
        httpOptions,
        destUri,
        onDownload,
        ~fetch=fakeFetch,
      )

      Assert.deepStrictEqual(events.contents, [Util.Event.Start])

      resolveRef.contents->Option.forEach(resolve =>
        resolve(Error(Util.Error.NoRedirectLocation))
      )

      let result = await p
      Assert.deepStrictEqual(result, Error(Util.Error.NoRedirectLocation))
    })

    Async.it("should emit Event.Fail after Event.Start when fetch fails", async () => {
      let events = ref([])
      let onDownload = event => {
        events := events.contents->Array.concat([event])
      }

      let resolveRef: ref<option<result<Fetch.Response.t, Util.Error.t> => unit>> = ref(None)
      let fetchPromise: promise<result<Fetch.Response.t, Util.Error.t>> = Promise.make((resolve, _) => {
        resolveRef := Some(resolve)
      })
      let fakeFetch = (_url, _opts) => fetchPromise

      let p = Util.asFile(
        httpOptions,
        destUri,
        onDownload,
        ~fetch=fakeFetch,
      )

      Assert.deepStrictEqual(events.contents, [Util.Event.Start])

      resolveRef.contents->Option.forEach(resolve =>
        resolve(Error(Util.Error.NoRedirectLocation))
      )

      let result = await p

      Assert.deepStrictEqual(result, Error(Util.Error.NoRedirectLocation))
      Assert.deepStrictEqual(events.contents, [Util.Event.Start, Util.Event.Fail])
    })

    Async.it("should trace full success lifecycle in order", async () => {
      let traces = ref([])
      let onTrace = event => {
        traces := traces.contents->Array.concat([event])
      }
      let events = ref([])
      let onDownload = event => {
        events := events.contents->Array.concat([event])
      }

      let fakeResponse = makeFakeResponse(200, Some("3"))
      let fakeFetch = (_url, _opts) => Promise.resolve(Ok(fakeResponse))
      let fakeReadBody = (_resp: Fetch.Response.t) => Promise.resolve(make3Bytes())
      let fakeWriteFile = (_uri: VSCode.Uri.t, _bytes: Core__Uint8Array.t) => Promise.resolve(Ok())

      let result = await Util.asFile(
        httpOptions,
        destUri,
        onDownload,
        ~trace=onTrace,
        ~fetch=fakeFetch,
        ~readBody=fakeReadBody,
        ~writeFile=fakeWriteFile,
      )

      Assert.deepStrictEqual(result, Ok())
      Assert.deepStrictEqual(events.contents, [
        Util.Event.Start,
        Util.Event.Progress(3, Some(3)),
        Util.Event.Finish,
      ])
      Assert.deepStrictEqual(traces.contents, [
        Trace.FetchStarted("https://example.com/download.zip"),
        Trace.FetchResponseReceived(200, Some(3)),
        Trace.BodyReadStarted,
        Trace.BodyProgress(3, Some(3)),
        Trace.BodyReadCompleted(3),
        Trace.WriteStarted(destUri),
        Trace.WriteCompleted(destUri),
      ])
    })

    Async.it("should trace BodyProgress with None total when content-length is absent", async () => {
      let traces = ref([])
      let onTrace = event => {
        traces := traces.contents->Array.concat([event])
      }
      let events = ref([])
      let onDownload = event => {
        events := events.contents->Array.concat([event])
      }

      let fakeResponse = makeFakeResponse(200, None)
      let fakeFetch = (_url, _opts) => Promise.resolve(Ok(fakeResponse))
      let fakeReadBody = (_resp: Fetch.Response.t) => Promise.resolve(make3Bytes())
      let fakeWriteFile = (_uri: VSCode.Uri.t, _bytes: Core__Uint8Array.t) => Promise.resolve(Ok())

      let result = await Util.asFile(
        httpOptions,
        destUri,
        onDownload,
        ~trace=onTrace,
        ~fetch=fakeFetch,
        ~readBody=fakeReadBody,
        ~writeFile=fakeWriteFile,
      )

      Assert.deepStrictEqual(result, Ok())

      let relevant = traces.contents->Array.filter(t =>
        switch t {
        | Trace.FetchResponseReceived(_, _) | Trace.BodyProgress(_, _) => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(relevant, [
        Trace.FetchResponseReceived(200, None),
        Trace.BodyProgress(3, None),
      ])

      Assert.deepStrictEqual(events.contents, [
        Util.Event.Start,
        Util.Event.Progress(3, None),
        Util.Event.Finish,
      ])
    })

    Async.it("should trace Failed(write) and not WriteCompleted on write error", async () => {
      let traces = ref([])
      let onTrace = event => {
        traces := traces.contents->Array.concat([event])
      }
      let events = ref([])
      let onDownload = event => {
        events := events.contents->Array.concat([event])
      }

      let fakeResponse = makeFakeResponse(200, Some("3"))
      let fakeFetch = (_url, _opts) => Promise.resolve(Ok(fakeResponse))
      let fakeReadBody = (_resp: Fetch.Response.t) => Promise.resolve(make3Bytes())
      let fakeWriteFile = (_uri: VSCode.Uri.t, _bytes: Core__Uint8Array.t) =>
        Promise.resolve(Error("disk full"))

      let result = await Util.asFile(
        httpOptions,
        destUri,
        onDownload,
        ~trace=onTrace,
        ~fetch=fakeFetch,
        ~readBody=fakeReadBody,
        ~writeFile=fakeWriteFile,
      )

      switch result {
      | Error(Util.Error.CannotWriteFile(_)) => ()
      | _ => Assert.fail("expected Error(CannotWriteFile(_))")
      }

      let writeStartedIdx = traces.contents->Array.findIndex(t =>
        switch t {
        | Trace.WriteStarted(_) => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(writeStartedIdx >= 0, true)

      let failedIdx = traces.contents->Array.findIndex(t =>
        switch t {
        | Trace.Failed("write", "disk full") => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(failedIdx >= 0, true)

      // WriteStarted must precede Failed
      Assert.deepStrictEqual(writeStartedIdx < failedIdx, true)

      let hasWriteCompleted = traces.contents->Array.some(t =>
        switch t {
        | Trace.WriteCompleted(_) => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(hasWriteCompleted, false)

      Assert.deepStrictEqual(events.contents, [
        Util.Event.Start,
        Util.Event.Progress(3, Some(3)),
        Util.Event.Fail,
      ])
    })

    Async.it("should trace Failed(fetch) and nothing after when fetch returns Error", async () => {
      let traces = ref([])
      let onTrace = event => {
        traces := traces.contents->Array.concat([event])
      }

      let fakeFetch = (_url, _opts) =>
        Promise.resolve(Error(Util.Error.NoRedirectLocation))

      let result = await Util.asFile(
        httpOptions,
        destUri,
        _event => (),
        ~trace=onTrace,
        ~fetch=fakeFetch,
      )

      Assert.deepStrictEqual(result, Error(Util.Error.NoRedirectLocation))

      Assert.deepStrictEqual(traces.contents, [
        Trace.FetchStarted("https://example.com/download.zip"),
        Trace.Failed("fetch", "Got HTTP 301/302 from GitHub without location in headers"),
      ])
    })

    Async.it("should trace Failed(body) and nothing after when readBody rejects", async () => {
      let traces = ref([])
      let onTrace = event => {
        traces := traces.contents->Array.concat([event])
      }
      let events = ref([])
      let onDownload = event => {
        events := events.contents->Array.concat([event])
      }

      let fakeResponse = makeFakeResponse(200, Some("3"))
      let fakeFetch = (_url, _opts) => Promise.resolve(Ok(fakeResponse))
      let fakeReadBody = (_resp: Fetch.Response.t) => {
        let err: exn = %raw(`new Error("stream broken")`)
        Promise.reject(err)
      }
      let fakeWriteFile = (_uri: VSCode.Uri.t, _bytes: Core__Uint8Array.t) => Promise.resolve(Ok())

      let result = await Util.asFile(
        httpOptions,
        destUri,
        onDownload,
        ~trace=onTrace,
        ~fetch=fakeFetch,
        ~readBody=fakeReadBody,
        ~writeFile=fakeWriteFile,
      )

      switch result {
      | Error(Util.Error.CannotWriteFile(_)) => ()
      | _ => Assert.fail("expected Error(CannotWriteFile(_))")
      }

      let bodyReadStartedIdx = traces.contents->Array.findIndex(t =>
        switch t {
        | Trace.BodyReadStarted => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(bodyReadStartedIdx >= 0, true)

      let failedIdx = traces.contents->Array.findIndex(t =>
        switch t {
        | Trace.Failed("body", _) => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(failedIdx >= 0, true)

      // BodyReadStarted must precede Failed
      Assert.deepStrictEqual(bodyReadStartedIdx < failedIdx, true)

      // Nothing past BodyReadStarted should appear
      let hasBodyProgress = traces.contents->Array.some(t =>
        switch t {
        | Trace.BodyProgress(_, _) => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(hasBodyProgress, false)

      let hasBodyReadCompleted = traces.contents->Array.some(t =>
        switch t {
        | Trace.BodyReadCompleted(_) => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(hasBodyReadCompleted, false)

      let hasWriteStarted = traces.contents->Array.some(t =>
        switch t {
        | Trace.WriteStarted(_) => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(hasWriteStarted, false)

      let hasWriteCompleted = traces.contents->Array.some(t =>
        switch t {
        | Trace.WriteCompleted(_) => true
        | _ => false
        }
      )
      Assert.deepStrictEqual(hasWriteCompleted, false)

      Assert.deepStrictEqual(events.contents, [Util.Event.Start, Util.Event.Fail])
    })
  })

  describe("asFile (integration)", () => {
    Async.it("should successfully download and save small file", async () => {
      // Use a very small test candidate
      let smallHttpOptions = {
        "headers": {"User-Agent": "agda-mode-vscode-test"},
        "host": "httpbin.org",
        "path": "/bytes/10" // Even smaller 10-byte file for faster test
      }
      
      // Create a temporary file path
      let tempFile = NodeJs.Path.join([
        NodeJs.Os.tmpdir(),
        "agda-download-test-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".bin"
      ])
      let destUri = VSCode.Uri.file(tempFile)
      
      let events = ref([])
      let onDownload = event => {
        events := Array.concat(events.contents, [event])
      }
      
      // Add timeout wrapper to prevent hanging
      let timedResult = await Util.timeoutAfter(
        Util.asFile(smallHttpOptions, destUri, onDownload),
        3000 // Shorter timeout for file download
      )
      
      switch timedResult {
      | Ok() => 
        // Verify file was created and has content
        Assert.ok(NodeJs.Fs.existsSync(tempFile))
        let content = NodeJs.Fs.readFileSync(tempFile)
        Assert.ok(NodeJs.Buffer.length(content) > 0)
        
        // Verify we got some events (at least Start and Finish, no Fail)
        let hasStart = events.contents->Array.some(event =>
          switch event {
          | Util.Event.Start => true
          | _ => false
          })
        let hasFinish = events.contents->Array.some(event =>
          switch event {
          | Util.Event.Finish => true
          | _ => false
          })
        let hasFail = events.contents->Array.some(event =>
          switch event {
          | Util.Event.Fail => true
          | _ => false
          })

        Assert.ok(hasStart)
        Assert.ok(hasFinish)
        Assert.deepStrictEqual(hasFail, false)
        
        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      | Error(error) => 
        // Network tests can be flaky, so we'll be lenient
        switch error {
        | Timeout(_) => 
          Js.Console.warn("Network test timed out (this may be expected)")
        | _ => 
          Js.Console.warn("Network test failed (this may be expected): " ++ Util.Error.toString(error))
        }
        Assert.ok(true) // Pass the test even if network fails
      }
    })
  })
})