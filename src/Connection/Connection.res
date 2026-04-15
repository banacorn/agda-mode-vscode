module Error = Connection__Error
module Agda = Connection__Endpoint__Agda
module ALS = Connection__Endpoint__ALS
module URI = Connection__URI
module Candidate = Connection__Candidate

module type Module = {
  type agdaVersion = string // Agda version
  type alsVersionInfo = {
    agdaVersion: string,
    alsVersion: option<string>,
    lspOptions: option<Connection__Protocol__LSP__Binding.executableOptions>,
  }
  type t =
    | Agda(Agda.t, string, agdaVersion) // connection, path, version
    | ALS(ALS.t, string, alsVersionInfo) // connection, path, version
    | ALSWASM(ALS.t, WASMLoader.t, string, alsVersionInfo) // connection, WASM loader, path, version

  // lifecycle
  let make: (string, Error.Establish.pathSource) => promise<result<t, Error.Establish.t>>
  let makeWithFallback: (
    Platform.t,
    Memento.t,
    VSCode.Uri.t,
    array<string>,
    array<string>,
    Chan.t<Log.t>,
  ) => promise<result<t, Error.t>>
  let toString: t => string
  let getPath: t => string
  let destroy: (option<t>, Chan.t<Log.t>) => promise<result<unit, Error.t>>

  let fromPathsOrCommands: (
    Platform.t,
    array<(string, Error.Establish.pathSource)>,
  ) => promise<result<t, Error.Establish.t>>
  let fromDownloads: (Platform.t, Memento.t, VSCode.Uri.t) => promise<result<t, Error.Establish.t>>

  // messaging
  let sendRequest: (
    t,
    VSCode.TextDocument.t,
    Request.t,
    Response.t => promise<unit>,
  ) => promise<result<unit, Error.t>>

  // utility
  let checkForPrebuiltDataDirectory: string => promise<option<string>>

  type probeResult =
    | IsAgda(string) // Agda version
    | IsALS(string, string, option<Connection__Protocol__LSP__Binding.executableOptions>) // ALS version, Agda version, LSP options
    | IsALSWASM(VSCode.Uri.t) // ALS WASM file
  let probeFilepath: string => promise<result<(string, probeResult), Error.Probe.t>>
  let probeCandidate: (
    Platform.t,
    Candidate.t,
  ) => promise<result<(Candidate.Resolved.t, probeResult), Error.Establish.t>>
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

  type agdaVersion = string // Agda version
  type alsVersionInfo = {
    agdaVersion: string,
    alsVersion: option<string>,
    lspOptions: option<Connection__Protocol__LSP__Binding.executableOptions>,
  }
  type t =
    | Agda(Agda.t, string, agdaVersion) // connection, path, version
    | ALS(ALS.t, string, alsVersionInfo) // connection, path, version
    | ALSWASM(ALS.t, WASMLoader.t, string, alsVersionInfo) // connection, WASM loader, path, version

  let toString = connection =>
    switch connection {
    | Agda(_, _, version) => "Agda v" ++ version
    | ALS(_, _, {alsVersion: Some(v), agdaVersion}) =>
      "Agda v" ++ agdaVersion ++ " Language Server v" ++ v
    | ALS(_, _, {alsVersion: None, agdaVersion}) =>
      "Agda v" ++ agdaVersion ++ " Language Server (version unknown)"
    | ALSWASM(_, _, _, {alsVersion: Some(v), agdaVersion}) =>
      "Agda v" ++ agdaVersion ++ " Language Server v" ++ v ++ " WASM"
    | ALSWASM(_, _, _, {alsVersion: None, agdaVersion}) =>
      "Agda v" ++ agdaVersion ++ " Language Server (version unknown) WASM"
    }

  let getPath = connection =>
    switch connection {
    | Agda(_, path, _) => path
    | ALS(_, path, _) => path
    | ALSWASM(_, _, path, _) => path
    }

  let destroy = async (connection, logChannel: Chan.t<Log.t>) =>
    switch connection {
    | None => Ok()
    | Some(connection) =>
      // Log disconnection before destroying
      logChannel->Chan.emit(Log.Connection(Disconnected(getPath(connection))))

      switch connection {
      | Agda(conn, _, _) =>
        await Agda.destroy(conn)
        Ok()
      | ALS(conn, _, _) =>
        switch await ALS.destroy(conn) {
        | Error(error) => Error(Error.CommWithALS(error))
        | Ok(_) => Ok()
        }
      | ALSWASM(_) =>
        // TODO: implement WASM connection destruction
        Ok()
      }
    }

  // Helper function to check for prebuilt data directory
  let checkForPrebuiltDataDirectory = async (executablePath: string) => {
    // the executable needs to be accompanied by a `data` directory
    // which can be specified by the environment variable "Agda_datadir"
    // prebuilt executables on GitHub have this directory placed alongside the executable
    let prebuildDataDirPath = NodeJs.Path.join([executablePath, "..", "data"])
    let prebuildDataDirURI = VSCode.Uri.file(prebuildDataDirPath)

    switch await FS.stat(prebuildDataDirURI) {
    | Ok(_) => Some(NodeJs.Path.join([executablePath, "..", "data"]))
    | Error(_) => None
    }
  }

  type probeResult =
    | IsAgda(string) // Agda version
    | IsALS(string, string, option<Connection__Protocol__LSP__Binding.executableOptions>) // ALS version, Agda version, LSP options
    | IsALSWASM(VSCode.Uri.t) // ALS WASM file

  let resolvedPathForErrors = (resolved: Candidate.Resolved.t): string =>
    if VSCode.Uri.scheme(resolved.resource) == "file" {
      VSCode.Uri.fsPath(resolved.resource)
    } else {
      VSCode.Uri.toString(resolved.resource)
    }

  let resolvedFromRawResource = raw => {
    let resource = switch URI.parse(raw) {
    | FileURI(_, resource) => resource
    }
    let resolved: Candidate.Resolved.t = {original: Candidate.Resource(resource), resource}
    resolved
  }

  let probeResolved = async (resolved: Candidate.Resolved.t) => {
    let {resource} = resolved
    let resourceString = resource->VSCode.Uri.toString
    // Check if it's a WASM file first (assume all WASM files are ALS)
    // Use URI string for .wasm check since fsPath may mangle non-file schemes (e.g. vscode-userdata:)
    if String.endsWith(resourceString, ".wasm") {
      // For file:// scheme (desktop), use fsPath as the path key
      // For other schemes (web), use the URI string to preserve the scheme
      let pathKey = if VSCode.Uri.scheme(resource) == "file" {
        VSCode.Uri.fsPath(resource)
      } else {
        resourceString
      }
      Ok(pathKey, IsALSWASM(resource))
    } else {
      // IMPORTANT: Convert URI to platform-specific file system path
      // VSCode.Uri.fsPath() handles cross-platform path conversion:
      // - On Windows: "/d/path/file" -> "d:\path\file"
      // - On Unix: "/path/file" -> "/path/file" (unchanged)
      // Always use fsPath (not the original path) for process spawning on Windows
      let fsPath = VSCode.Uri.fsPath(resource)
      let result = await Connection__Process__Exec.run(fsPath, ["--version"], ~timeout=3000)
      switch result {
      | Ok(output) =>
        // try Agda
        switch String.match(output, %re("/Agda version (.*)/")) {
        | Some([_, Some(version)]) => Ok(fsPath, IsAgda(version))
        | _ =>
          // try ALS
          switch String.match(output, %re("/Agda v(.*) Language Server v(.*)/")) {
          | Some([_, Some(agdaVersion), Some(alsVersion)]) =>
            let lspOptions = switch await checkForPrebuiltDataDirectory(fsPath) {
            | Some(assetPath) =>
              let env = Dict.fromArray([("Agda_datadir", assetPath)])
              Some({Connection__Protocol__LSP__Binding.env: env})
            | None => None
            }
            Ok(fsPath, IsALS(alsVersion, agdaVersion, lspOptions))
          | _ => Error(Error.Probe.NotAgdaOrALS(output))
          }
        }
      | Error(error) => Error(Error.Probe.CannotDetermineAgdaOrALS(error))
      }
    }
  }

  // see if it's a Agda executable or a language server
  let probeFilepath = async path => await probeResolved(resolvedFromRawResource(path))

  let probeCandidate = async (
    platformDeps: Platform.t,
    candidate: Candidate.t,
  ): result<(Candidate.Resolved.t, probeResult), Error.Establish.t> => {
    module PlatformOps = unpack(platformDeps)
    switch await Candidate.resolve(PlatformOps.findCommand, candidate) {
    | Ok(resolved) =>
      let source = switch resolved.original {
      | Candidate.Command(command) => Error.Establish.FromCommandLookup(command)
      | Candidate.Resource(_) => Error.Establish.FromConfig
      }
      switch await probeResolved(resolved) {
      | Ok(_, probeResult) => Ok((resolved, probeResult))
      | Error(error) =>
        Error(
          Error.Establish.fromProbeError(resolvedPathForErrors(resolved), error, source),
        )
      }
    | Error(commandError) =>
      switch candidate {
      | Candidate.Command(command) => Error(Error.Establish.fromCommandError(command, commandError))
      | Candidate.Resource(_) =>
        raise(Failure("Connection__Candidate.resolve returned Error for Resource candidate"))
      }
    }
  }

  let makeResolved = async (
    resolved: Candidate.Resolved.t,
    source: Error.Establish.pathSource,
    ~pathForErrors: string=resolvedPathForErrors(resolved),
  ): result<
    t,
    Error.Establish.t,
  > => {
    switch await probeResolved(resolved) {
    | Ok(path, IsAgda(agdaVersion)) =>
      let connection = await Agda.make(path, agdaVersion)
      Ok(Agda(connection, path, agdaVersion))
    | Ok(path, IsALS(alsVersion, agdaVersion, lspOptions)) =>
      switch await ALS.make(
        Connection__Transport.ViaPipe(path, []),
        lspOptions,
        InitOptions.getFromConfig(),
      ) {
      | Error(error) =>
        Error(Error.Establish.fromProbeError(path, CannotMakeConnectionWithALS(error), source))
      | Ok(conn) =>
        Ok(ALS(conn, path, {agdaVersion, alsVersion: Some(alsVersion), lspOptions}))
      }
    | Ok(path, IsALSWASM(uri)) =>
      // Get the WASM loader extension
      switch VSCode.Extensions.getExtension("qbane.als-wasm-loader") {
      | None =>
        Error(
          Error.Establish.fromProbeError(
            path,
            Error.Probe.CannotMakeConnectionWithALSWASMYet(
              "Extension 'qbane.als-wasm-loader' not found. Either it is not installed or is disabled.",
            ),
            source,
          ),
        )
      | Some(extension) =>
        try {
          // Ensure extension is activated
          if !VSCode.Extension.isActive(extension) {
            let _ = await VSCode.Extension.activate(extension)
          }

          switch await FS.readFile(uri) {
          | Error(error) =>
            Error(
              Error.Establish.fromProbeError(
                path,
                Error.Probe.CannotMakeConnectionWithALSWASMYet("Failed to read WASM file: " ++ error),
                source,
              ),
            )
          | Ok(raw) =>
            let wasmLoader = await WASMLoader.make(extension, raw)

            // No Agda data directory preparation needed for WASM 2.8.0+
            // (data is already bundled in the WASM binary)
            let isDesktop = %raw("Vscode.env.uiKind === Vscode.UIKind.Desktop")
            let sabAvailable = %raw("typeof SharedArrayBuffer !== 'undefined'")
            let coi = %raw("typeof crossOriginIsolated !== 'undefined' ? crossOriginIsolated : false")
            if !isDesktop && (!sabAvailable || !coi) {
              let reason =
                if !sabAvailable {
                  "SharedArrayBuffer is not available in this browser environment."
                } else {
                  "The page is not crossOriginIsolated, which is required to enable SharedArrayBuffer."
                }
              Error(
                Error.Establish.fromProbeError(
                  path,
                  Error.Probe.CannotMakeConnectionWithALSWASMYet(
                    reason
                    ++
                    " Please serve the web extension with Cross-Origin Opener Policy (COOP) and Cross-Origin Embedder Policy (COEP) headers.",
                  ),
                  source,
                ),
              )
            } else {
            switch await ALS.make(
              Connection__Transport.ViaWASM(wasmLoader),
              None,
              InitOptions.getFromConfig(),
            ) {
            | Error(error) =>
              Error(
                Error.Establish.fromProbeError(path, CannotMakeConnectionWithALS(error), source),
              )
            | Ok(conn) =>
              Ok(
                ALSWASM(
                  conn,
                  wasmLoader,
                  path,
                  {agdaVersion: conn.agdaVersion, alsVersion: conn.alsVersion, lspOptions: None},
                ),
              )
            }
            }
          }
        } catch {
        | Exn.Error(error) =>
          let errorMessage = Js.Exn.message(error)->Option.getOr("Unknown error")
          Error(
            Error.Establish.fromProbeError(
              path,
              Error.Probe.CannotMakeConnectionWithALSWASMYet("Unexpected error: " ++ errorMessage),
              source,
            ),
          )
        }
      }
    | Error(error) => Error(Error.Establish.fromProbeError(pathForErrors, error, source))
    }
  }

  let make = async (rawpath: string, source: Error.Establish.pathSource): result<
    t,
    Error.Establish.t,
  > => await makeResolved(resolvedFromRawResource(rawpath), source, ~pathForErrors=rawpath)

  // Combinator: try each task in array until one succeeds, or return all failures
  let tryUntilSuccess = async (xs: array<unit => promise<result<'b, 'e>>>): result<
    'b,
    array<'e>,
  > => {
    let rec loop = async (remaining, errors) => {
      switch remaining[0] {
      | Some(task) =>
        switch await task() {
        | Ok(success) => Ok(success)
        | Error(error) =>
          let newErrors = errors->Array.concat([error])
          let nextTasks = remaining->Array.sliceToEnd(~start=1)
          await loop(nextTasks, newErrors)
        }
      | None => Error(errors)
      }
    }
    await loop(xs, [])
  }

  let tryCandidate = async (
    platformDeps: Platform.t,
    candidate: Candidate.t,
    source: Error.Establish.pathSource,
  ): result<t, Error.Establish.t> => {
    module PlatformOps = unpack(platformDeps)
    switch await Candidate.resolve(PlatformOps.findCommand, candidate) {
    | Ok(resolved) =>
      let resolvedSource = switch resolved.original {
      | Candidate.Command(command) => Error.Establish.FromCommandLookup(command)
      | Candidate.Resource(_) => source
      }
      await makeResolved(resolved, resolvedSource)
    | Error(commandError) =>
      switch candidate {
      | Candidate.Command(command) => Error(Error.Establish.fromCommandError(command, commandError))
      | Candidate.Resource(_) =>
        raise(Failure("Connection__Candidate.resolve returned Error for Resource candidate"))
      }
    }
  }

  // Make a connection by interpreting each entry as a command or a path.
  let fromPathsOrCommands = async (
    platformDeps: Platform.t,
    entries: array<(string, Error.Establish.pathSource)>,
  ): result<t, Error.Establish.t> => {
    let tasks = entries->Array.map(((raw, source)) => {
      let candidate = Candidate.make(raw)
      () => tryCandidate(platformDeps, candidate, source)
    })

    switch await tryUntilSuccess(tasks) {
    | Ok(connection) => Ok(connection)
    | Error(errors) => Error(Error.Establish.mergeMany(errors))
    }
  }

  // Try to download ALS and create connection directly, with the following steps:
  // 1. Check platform support
  // 2. Check download policy
  // 3. Check if already downloaded or download latest ALS
  // 4. Create connection directly from downloaded raw path
  // Returns Ok(connection) on success, or Error with failure details
  let fromDownloads = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
  ): result<t, Error.Establish.t> => {
    module PlatformOps = unpack(platformDeps)

    switch await PlatformOps.determinePlatform() {
    | Error(platform) => Error(Error.Establish.fromDownloadError(PlatformNotSupported(platform)))
    | Ok(platform) =>
      // On web, always download (no alternatives exist)
      // On desktop, respect user's config
      let policy = switch platform {
      | Connection__Download__Platform.Web =>
          Config.Connection.DownloadPolicy.Yes
      | _ =>
          switch Config.Connection.DownloadPolicy.get() {
          | Undecided => await PlatformOps.askUserAboutDownloadPolicy()
          | policy => policy
          }
      }

      Util.log("[ debug ] fromDownloads: policy = " ++ Config.Connection.DownloadPolicy.toString(policy), "")

      switch policy {
      | Config.Connection.DownloadPolicy.Undecided =>
        // User cancelled, treat as No
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Establish.fromDownloadError(Connection__Download.Error.OptedNotToDownload))
      | No =>
        // User opted not to download, set policy and return error
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Establish.fromDownloadError(Connection__Download.Error.OptedNotToDownload))
      | Yes =>
        await Config.Connection.DownloadPolicy.set(Yes)
        // Use the selected channel from memento, defaulting to DevALS.
        // Stale memento values are clamped to DevALS.
        let channel = switch platform {
        | Connection__Download__Platform.Web => Connection__Download.Channel.DevALS
        | _ =>
          switch Memento.SelectedChannel.get(memento) {
          | Some("DevALS") => Connection__Download.Channel.DevALS
          | _ => Connection__Download.Channel.DevALS
          }
        }
        // WASM fallback source is populated when resolveDownloadChannel returns a FromGitHub descriptor;
        // it picks the first WASM asset from the same release so the fallback stays release-managed.
        let wasmFallbackSource: ref<option<Connection__Download.Source.t>> = ref(None)
        let tryWasmFallback = async () =>
          switch wasmFallbackSource.contents {
          | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          | Some(wasmSource) =>
            switch await PlatformOps.download(globalStorageUri, wasmSource) {
            | Ok(path) => Ok(path)
            | Error(error) => Error(error)
            }
          }
        let tryDownloadSource = async source =>
          switch await PlatformOps.download(globalStorageUri, source) {
          | Ok(path) => Ok(path)
          | Error(error) =>
            // For native downloads on any channel, retry once with WASM.
            // This gives desktop a graceful fallback when native artifacts fail.
            switch source {
            | Connection__Download.Source.FromURL(_, url, _) =>
              if url->String.endsWith(".wasm") {
                Error(error)
              } else {
                await tryWasmFallback()
              }
            | Connection__Download.Source.FromGitHub(_, descriptor) =>
              if descriptor.asset.name->String.includes("wasm") {
                Error(error)
              } else {
                await tryWasmFallback()
              }
            }
          }
        // Get the downloaded path, download if not already done
        let downloadResult = switch await PlatformOps.alreadyDownloaded(
          globalStorageUri,
          channel,
        ) {
        | Some(path) =>
          Util.log("[ debug ] fromDownloads: alreadyDownloaded = Some", path)
          Ok(path)
        | None =>
          Util.log("[ debug ] fromDownloads: alreadyDownloaded = None", "")
          switch await PlatformOps.resolveDownloadChannel(channel, true)(
            memento,
            globalStorageUri,
            platform,
          ) {
          | Error(resolveError) =>
            Error(Error.Establish.fromDownloadError(resolveError))
          | Ok(Connection__Download.Source.FromGitHub(_, descriptor) as source) =>
            let nativeAgdaVersion =
              Connection__Download.DownloadArtifact.parseName(descriptor.asset.name)
              ->Option.map(a => a.agdaVersion)
            wasmFallbackSource :=
              Connection__DevALS.allWasmAssets(descriptor.release)
              ->Array.find(asset =>
                switch (nativeAgdaVersion, Connection__Download.DownloadArtifact.parseName(asset.name)) {
                | (Some(nv), Some(wa)) => nv == wa.agdaVersion
                | _ => false
                }
              )
              ->Option.map(asset =>
                Connection__Download.Source.FromGitHub(channel, {
                  Connection__Download__GitHub.DownloadDescriptor.asset: asset,
                  release: descriptor.release,
                  saveAsFileName: descriptor.saveAsFileName,
                })
              )
            switch await tryDownloadSource(source) {
            | Error(error) => Error(Error.Establish.fromDownloadError(error))
            | Ok(path) => Ok(path)
            }
          | Ok(Connection__Download.Source.FromURL(_, _, _) as source) =>
            switch await tryDownloadSource(source) {
            | Error(error) => Error(Error.Establish.fromDownloadError(error))
            | Ok(path) => Ok(path)
            }
          }
        }

        switch downloadResult {
        | Ok(path) =>
          switch await make(path, FromDownload(channel)) {
          | Ok(connection) => Ok(connection)
          | Error(nativeError) =>
            // Native path failed to connect — try WASM fallback on desktop
            if !(path->String.endsWith(".wasm")) {
              switch await tryWasmFallback() {
              | Ok(wasmPath) =>
                switch await make(wasmPath, FromDownload(channel)) {
                | Ok(connection) => Ok(connection)
                | Error(wasmError) =>
                  Error(Error.Establish.merge(
                    {...nativeError, download: Succeeded},
                    {...wasmError, download: Succeeded},
                  ))
                }
              | Error(_) =>
                Error({...nativeError, download: Succeeded})
              }
            } else {
              Error({...nativeError, download: Succeeded})
            }
          }
        | Error(error) => Error(error)
        }
      }
    }
  }

  // Make a connection to Agda or ALS by trying:
  //  0. The previously selected path (if any)
  //  1. Each remaining path from user config in reverse order (last entry = highest priority)
  //  2. Download fallback
  //
  // Config modification: download fallback prepends the downloaded path (lowest priority).
  // PreferredCandidate is never modified here — only explicit user actions may set it.

  let makeWithFallback = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
    paths: array<string>,
    _commands: array<string>,
    logChannel: Chan.t<Log.t>,
  ): result<t, Error.t> => {
    let logConnection = connection => {
      switch connection {
      | Agda(_, path, version) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToAgda(path, version)))
      | ALS(_, path, {alsVersion: Some(v), agdaVersion}) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, Some(v, agdaVersion))))
      | ALS(_, path, {alsVersion: None}) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, None)))
      | ALSWASM(_, _, path, {alsVersion: Some(v), agdaVersion}) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, Some(v, agdaVersion))))
      | ALSWASM(_, _, path, {alsVersion: None}) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, None)))
      }
    }

    let preferredCandidate = Memento.PreferredCandidate.get(memento)

    // Step 0: preferred candidate, if present.
    let preferredEntries = switch preferredCandidate {
    | Some(path) => [(path, Error.Establish.FromConfig)]
    | None => []
    }

    // Step 1: remaining config entries in reverse order (last entry = highest priority).
    let remainingPaths = switch preferredCandidate {
    | Some(preferredPath) => paths->Array.filter(path => path !== preferredPath)
    | None => paths
    }
    let pathsWithSource =
      remainingPaths
      ->Array.toReversed
      ->Array.map(path => (path, Error.Establish.FromConfig))

    // Try step 0 (preferred candidate) -> step 1 (config paths) -> download fallback.
    // Command probes are not part of the resolution chain.
    switch await fromPathsOrCommands(platformDeps, preferredEntries) {
    | Ok(connection) =>
      logConnection(connection)
      Ok(connection)
    | Error(step0Error) =>
      switch await fromPathsOrCommands(platformDeps, pathsWithSource) {
      | Ok(connection) =>
        logConnection(connection)
        Ok(connection)
      | Error(step1Error) =>
        let pathErrors = Error.Establish.mergeMany([step0Error, step1Error])
      switch await fromDownloads(platformDeps, memento, globalStorageUri) {
      | Ok(connection) =>
        logConnection(connection)
        // Automatic fallback: prepend (lowest priority), do NOT set PreferredCandidate
        let downloadedPath = getPath(connection)
        if !(paths->Array.includes(downloadedPath)) {
          await Config.Connection.setAgdaPaths(
            logChannel,
            Array.concat([downloadedPath], paths),
          )
        }
        Ok(connection)
      | Error(downloadError) =>
        Error(Establish(Error.Establish.mergeMany([pathErrors, downloadError])))
      }
      }
    }
  }

  let sendRequest = async (connection, document, request, handler) => {
    // encode the Request to some string (native paths)
    let encodeRequest = (document, version) => {
      let filepath = document->VSCode.TextDocument.fileName->Parser.filepath
      let libraryPath = Config.getLibraryPath()
      let highlightingMethod = Config.Highlighting.getHighlightingMethod()
      let backend = Config.getBackend()
      Request.encode(document, version, filepath, backend, libraryPath, highlightingMethod, request)
    }

    // encode the Request for WASM: map VS Code URI to WASI /workspace path
    let encodeRequestForWASM = (wasmLoader: WASMLoader.t, document, version) => {
      let libraryPath = Config.getLibraryPath()
      let highlightingMethod = Config.Highlighting.getHighlightingMethod()
      let backend = Config.getBackend()

      // Use the loader-provided URI converters to map code URIs to protocol URIs
      let codeUri = document->VSCode.TextDocument.uri
      let converters = wasmLoader.createUriConverters()
      // code2Protocol returns a string like "file:///workspace/..."
      let protoUri: string = %raw("function(conv, uri){ return conv.code2Protocol(uri); }")(
        converters,
        codeUri,
      )
      // Extract a POSIX path from the file URI (e.g. /workspace/..), fallback to the original if parsing fails
      let filepath: string = try {
        VSCode.Uri.parse(protoUri)->VSCode.Uri.path
      } catch {
      | Exn.Error(_) => protoUri
      }

      let workspaceFolders = VSCode.Workspace.workspaceFolders->Option.getOr([])

      // Ensure WASM-visible files live under /workspace. For URIs that map to
      // "/workspace<something>" we insert the missing slash so WASI resolves it.
      let filepath = {
        let prefix = if Array.length(workspaceFolders) === 1 { "/workspace" } else { "/workspaces" }
        if filepath->String.startsWith(prefix) {
          let rest = filepath->String.sliceToEnd(~start=String.length(prefix))
          if rest == "" || filepath->String.startsWith(prefix ++ "/") {
            filepath
          } else {
            prefix ++ "/" ++ rest
          }
        } else {
          filepath
        }
      }

      Request.encode(document, version, filepath, backend, libraryPath, highlightingMethod, request)
    }

    switch connection {
    | ALS(conn, _path, _versionInfo) =>
      switch await ALS.sendRequest(conn, encodeRequest(document, conn.agdaVersion), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(connection), Chan.make())
        Error(Error.CommWithALS(error))
      | Ok() => Ok()
      }
    | Agda(conn, _path, version) =>
      switch await Agda.sendRequest(conn, encodeRequest(document, version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(connection), Chan.make())
        Error(Error.CommWithAgda(error))
      | Ok() => Ok()
      }
    | ALSWASM(conn, wasmLoader, _path, _version) =>
      // Use the same ALS.sendRequest pattern as regular ALS connections
      let payload = encodeRequestForWASM(wasmLoader, document, conn.agdaVersion)
      switch await ALS.sendRequest(conn, payload, handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(connection), Chan.make())
        Error(Error.CommWithALS(error))
      | Ok() => Ok()
      }
    }
  }
}

include Module
