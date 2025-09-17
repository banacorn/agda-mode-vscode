module Error = Connection__Error
module Agda = Connection__Endpoint__Agda
module ALS = Connection__Endpoint__ALS
module URI = Connection__URI

module type Module = {
  type agdaVersion = string // Agda version
  type alsVersion = (string, string, option<Connection__Protocol__LSP__Binding.executableOptions>) // (ALS version, Agda version, LSP options)
  type t =
    | Agda(Agda.t, string, agdaVersion) // connection, path, version
    | ALS(ALS.t, string, option<alsVersion>) // connection, path, version
    | ALSWASM(ALS.t, WASMLoader.t, string, option<alsVersion>) // path, version

  // lifecycle
  let make: string => promise<result<t, Error.Establish.t>>
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

  let fromPaths: (Platform.t, array<string>) => promise<result<t, Error.Establish.t>>
  let fromCommands: (Platform.t, array<string>) => promise<result<t, Error.Establish.t>>
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
    | IsALSOfUnknownVersion(NodeJs.Url.t) // for TCP connections
    | IsALSWASM(VSCode.Uri.t) // ALS WASM file
  let probeFilepath: string => promise<result<(string, probeResult), Error.Probe.t>>
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
  type alsVersion = (string, string, option<Connection__Protocol__LSP__Binding.executableOptions>) // (ALS version, Agda version, LSP options)
  type t =
    | Agda(Agda.t, string, agdaVersion) // connection, path, version
    | ALS(ALS.t, string, option<alsVersion>) // connection, path, version
    | ALSWASM(ALS.t, WASMLoader.t, string, option<alsVersion>) // connection, WASM loader, path, version

  let toString = connection =>
    switch connection {
    | Agda(_, _, version) => "Agda v" ++ version
    | ALS(_, _, Some(alsVersion, agdaVersion, _)) =>
      "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion
    | ALS(_, _, None) => "Agda Language Server of unknown version"
    | ALSWASM(_, _, _, Some(alsVersion, agdaVersion, _)) =>
      "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion ++ " (WASM)"
    | ALSWASM(_, _, _, None) => "Agda Language Server of unknown version (WASM)"
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
    | IsALSOfUnknownVersion(NodeJs.Url.t) // for TCP connections
    | IsALSWASM(VSCode.Uri.t) // ALS WASM file
  // see if it's a Agda executable or a language server
  let probeFilepath = async path => {
    switch URI.parse(path) {
    | Connection__URI.LspURI(_, nodejsUrl) =>
      // probe with TCP first
      switch await Connection__Transport__TCP.probe(nodejsUrl) {
      | Ok() => Ok(path, IsALSOfUnknownVersion(nodejsUrl))
      | Error(Connection__Transport__TCP.Error.Timeout(timeout)) =>
        Error(Error.Probe.CannotMakeConnectionWithALS(ConnectionTimeoutError(timeout)))
      | Error(Connection__Transport__TCP.Error.OnError(exn)) =>
        Error(Error.Probe.CannotMakeConnectionWithALS(ConnectionError(exn)))
      }
    | FileURI(_, vscodeUri) =>
      // IMPORTANT: Convert URI to platform-specific file system path
      // VSCode.Uri.fsPath() handles cross-platform path conversion:
      // - On Windows: "/d/path/file" -> "d:\path\file"
      // - On Unix: "/path/file" -> "/path/file" (unchanged)
      // Always use fsPath (not the original path) for all subsequent operations
      // to ensure proper process spawning on Windows
      let fsPath = VSCode.Uri.fsPath(vscodeUri)

      // Check if it's a WASM file first (assume all WASM files are ALS)
      if String.endsWith(fsPath, ".wasm") {
        Ok(fsPath, IsALSWASM(vscodeUri))
      } else {
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
  }

  let make = async (rawpath: string): result<t, Error.Establish.t> => {
    switch await probeFilepath(rawpath) {
    | Ok(path, IsAgda(agdaVersion)) =>
      let connection = await Agda.make(path, agdaVersion)
      Ok(Agda(connection, path, agdaVersion))
    | Ok(path, IsALS(alsVersion, agdaVersion, lspOptions)) =>
      switch await ALS.make(
        Connection__Transport.ViaPipe(rawpath, []),
        lspOptions,
        InitOptions.getFromConfig(),
      ) {
      | Error(error) =>
        Error(Error.Establish.fromProbeError(path, CannotMakeConnectionWithALS(error)))
      | Ok(conn) => Ok(ALS(conn, path, Some(alsVersion, agdaVersion, lspOptions)))
      }
    | Ok(path, IsALSOfUnknownVersion(url)) =>
      switch await ALS.make(
        Connection__Transport.ViaTCP(rawpath, url),
        None,
        InitOptions.getFromConfig(),
      ) {
      | Error(error) =>
        Error(Error.Establish.fromProbeError(path, CannotMakeConnectionWithALS(error)))
      | Ok(conn) =>
        switch conn.alsVersion {
        | None => Ok(ALS(conn, path, None)) // version unknown
        | Some(alsVersion) => Ok(ALS(conn, path, Some(alsVersion, conn.agdaVersion, None))) // version known
        }
      }
    | Ok(path, IsALSWASM(uri)) =>
      // Get the WASM loader extension
      switch VSCode.Extensions.getExtension("qbane.als-wasm-loader") {
      | None =>
        Error(Error.Establish.fromProbeError(path, Error.Probe.CannotMakeConnectionWithALSWASMYet))
      | Some(extension) =>
        try {
          // Ensure extension is activated
          if !VSCode.Extension.isActive(extension) {
            let _ = await VSCode.Extension.activate(extension)
          }

          switch await FS.readFile(uri) {
          | Error(error) =>
            Error(
              Error.Establish.fromProbeError(path, Error.Probe.CannotMakeConnectionWithALSWASMYet),
            )
          | Ok(raw) =>
            let wasmLoader = await WASMLoader.make(extension, raw)

            // Prepare the Agda data directory in the memory filesystem (provisional for Agda-2.7.0)
            switch await WASMLoader.prepareAgdaDataDir(extension, wasmLoader.memfsAgdaDataDir) {
            | Error(errorMsg) =>
              Error(
                Error.Establish.fromProbeError(
                  path,
                  Error.Probe.CannotMakeConnectionWithALSWASMYet,
                ),
              )
            | Ok(env) =>
              switch await ALS.make(
                Connection__Transport.ViaWASM(wasmLoader),
                Some({
                  Connection__Protocol__LSP__Binding.env: env,
                }),
                InitOptions.getFromConfig(),
              ) {
              | Error(error) =>
                Error(Error.Establish.fromProbeError(path, CannotMakeConnectionWithALS(error)))
              | Ok(conn) =>
                let version = switch conn.alsVersion {
                | None => None
                | Some(version) => Some(conn.agdaVersion, version, None)
                }
                Ok(ALSWASM(conn, wasmLoader, path, version))
              }
            }
          }
        } catch {
        | Exn.Error(error) =>
          Error(
            Error.Establish.fromProbeError(path, Error.Probe.CannotMakeConnectionWithALSWASMYet),
          )
        }
      }
    | Error(error) => Error(Error.Establish.fromProbeError(rawpath, error))
    }
  }

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

  // for tagging results of path discovery
  type from = FromPaths | FromCommands | FromDownloads

  let tagFrom = async (result: promise<result<t, Error.Establish.t>>, from: from): result<
    (t, from),
    Error.Establish.t,
  > => {
    switch await result {
    | Ok(connection) => Ok((connection, from))
    | Error(error) => Error(error)
    }
  }

  // Make a connection by trying all given paths in order
  let fromPaths = async (platformDeps: Platform.t, paths: array<string>): result<
    t,
    Error.Establish.t,
  > => {
    module PlatformOps = unpack(platformDeps)

    let tasks = paths->Array.map(path => () => make(path))
    switch await tryUntilSuccess(tasks) {
    | Ok(connection) => Ok(connection)
    | Error(pathErrors) => Error(Error.Establish.mergeMany(pathErrors))
    }
  }

  // Make a connection from commands, trying each command until one succeeds
  let fromCommands = async (platformDeps: Platform.t, commands: array<string>): result<
    t,
    Error.Establish.t,
  > => {
    module PlatformOps = unpack(platformDeps)

    let tryCommand = async (command): result<t, Error.Establish.t> => {
      switch await PlatformOps.findCommand(command) {
      | Ok(rawPath) => await make(rawPath)
      | Error(commandError) => Error(Error.Establish.fromCommandError(command, commandError))
      }
    }

    let tasks = commands->Array.map(command => () => tryCommand(command))
    switch await tryUntilSuccess(tasks) {
    | Ok(connection) => Ok(connection)
    | Error(errors) =>
      // merge all errors from `tryCommand`
      Error(errors->Error.Establish.mergeMany)
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
      // Check download policy
      let policy = switch Config.Connection.DownloadPolicy.get() {
      | Undecided => await PlatformOps.askUserAboutDownloadPolicy()
      | policy => policy
      }

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
        // Get the downloaded path, download if not already done
        let downloadResult = switch await PlatformOps.alreadyDownloaded(
          globalStorageUri,
          LatestALS,
        ) {
        | Some(path) => Ok(path)
        | None =>
          switch await PlatformOps.resolveDownloadOrder(LatestALS, true)(
            memento,
            globalStorageUri,
            platform,
          ) {
          | Error(error) => Error(Error.Establish.fromDownloadError(error))
          | Ok(downloadDescriptor) =>
            switch await PlatformOps.download(globalStorageUri, downloadDescriptor) {
            | Error(error) => Error(Error.Establish.fromDownloadError(error))
            | Ok(path) => Ok(path)
            }
          }
        }

        switch downloadResult {
        | Ok(path) =>
          switch await make(path) {
          | Ok(connection) => Ok(connection)
          | Error(error) => Error(error)
          }
        | Error(error) => Error(error)
        }
      }
    }
  }

  // Make a connection to Agda or ALS by trying:
  //  1. The previously selected path (only if it exists in user config)
  //  2. Each path from user config sequentially
  //  3. Each command from the commands array sequentially
  //  4. Downloading the latest ALS
  //
  // User config path modification policy:
  //  * Existing paths are never removed or modified programmatically
  //  * New paths are only added if:
  //    - User actively chooses a path from the switch version UI, OR
  //    - User has no config and system discovers a working path
  //
  // `Memento.PickedConnection` behavior:
  //  * Only used for prioritizing existing paths from user config
  //  * Ignored if it doesn't exist in user config
  //  * Always set to the working connection path

  let makeWithFallback = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
    paths: array<string>,
    commands: array<string>,
    logChannel: Chan.t<Log.t>,
  ): result<t, Error.t> => {
    let logConnection = connection => {
      switch connection {
      | Agda(_, path, version) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToAgda(path, version)))
      | ALS(_, path, Some(alsVersion, agdaVersion, _lspOptions)) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, Some(alsVersion, agdaVersion))))
      | ALS(_, path, None) => logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, None)))
      | ALSWASM(_, _, path, Some(alsVersion, agdaVersion, _)) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, Some(alsVersion, agdaVersion))))
      | ALSWASM(_, _, path, None) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, None)))
      }
    }

    let pathsWithSelectedConnection = switch Memento.PickedConnection.get(memento) {
    | Some(pickedPath) =>
      // Only prioritize picked path if it exists in user's configuration
      if paths->Array.includes(pickedPath) {
        [pickedPath]->Array.concat(paths->Array.filter(p => p !== pickedPath))
      } else {
        // If picked path is not in user config, use user config as-is
        paths
      }
    | None => paths
    }

    // Try each method in order
    let tasks = [
      () => fromPaths(platformDeps, pathsWithSelectedConnection)->tagFrom(FromPaths),
      () => fromCommands(platformDeps, commands)->tagFrom(FromCommands),
      () => fromDownloads(platformDeps, memento, globalStorageUri)->tagFrom(FromDownloads),
    ]

    switch await tryUntilSuccess(tasks) {
    | Ok(connection, from) =>
      logConnection(connection)
      // Only add to config if connection succeeded from fallback methods (not from config paths)
      switch from {
      | FromPaths => () // Never modify config when using existing config paths
      | FromCommands | FromDownloads =>
        await Config.Connection.addAgdaPath(logChannel, getPath(connection))
      }

      await Memento.PickedConnection.set(memento, Some(getPath(connection)))
      Ok(connection)
    | Error(errors) => Error(Establish(Error.Establish.mergeMany(errors)))
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
      let filepath: string = %raw(
        "function(u){ try { return new URL(u).pathname; } catch(e) { return u; } }"
      )(protoUri)

      Request.encode(document, version, filepath, backend, libraryPath, highlightingMethod, request)
    }

    switch connection {
    | ALS(conn, path, versionInfo) =>
      switch await ALS.sendRequest(conn, encodeRequest(document, conn.agdaVersion), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(connection), Chan.make())
        Error(Error.CommWithALS(error))
      | Ok() => Ok()
      }
    | Agda(conn, path, version) =>
      switch await Agda.sendRequest(conn, encodeRequest(document, version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(connection), Chan.make())
        Error(Error.CommWithAgda(error))
      | Ok() => Ok()
      }
    | ALSWASM(conn, wasmLoader, path, version) =>
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
