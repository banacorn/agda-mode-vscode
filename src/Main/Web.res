// Web-specific entry point that creates Platform.makeWeb() dependencies
// This file will be used for the web bundle

// Web implementation - provides mock/limited functionality for browser environment
module Web: Platform.PlatformOps = {
  let determinePlatform = () => {
    Promise.resolve(Ok(Connection__Download__Platform.Web))
  }

  let findCommand = (_command, ~timeout as _timeout=1000) =>
    Promise.resolve(Error(Connection__Command.Error.NotFound))

  let alreadyDownloaded = async (globalStorageUri, channel) => {
    let findNestedDownloaded = async (rootUri: VSCode.Uri.t, fileName: string): option<string> =>
      switch await FS.readDirectory(rootUri) {
      | Error(_) => None
      | Ok(entries) =>
        let sortedEntries = entries->Array.toSorted(((a, _), (b, _)) =>
          if a > b {
            -1.
          } else if a < b {
            1.
          } else {
            0.
          }
        )
        let rec loop = async i =>
          if i >= Array.length(sortedEntries) {
            None
          } else {
            let (name, fileType) = Belt.Array.getExn(sortedEntries, i)
            if fileType != VSCode.FileType.Directory {
              await loop(i + 1)
            } else {
              let candidateUri = VSCode.Uri.joinPath(rootUri, [name, fileName])
              switch await FS.stat(candidateUri) {
              | Ok(_) => Some(VSCode.Uri.toString(candidateUri))
              | Error(_) => await loop(i + 1)
              }
            }
          }
        await loop(0)
      }

    switch channel {
    | Connection__Download.Channel.LatestALS => {
        // Web doesn't support LatestALS (native binaries)
        None
      }
    | Connection__Download.Channel.DevALS => {
        // Web: Only check for WASM, ignore native binaries
        let wasmUri = VSCode.Uri.joinPath(globalStorageUri, ["dev-als", "als.wasm"])
        switch await FS.stat(wasmUri) {
        | Ok(_) => Some(VSCode.Uri.toString(wasmUri)) // Use URI string for WASM
        | Error(_) =>
          await findNestedDownloaded(VSCode.Uri.joinPath(globalStorageUri, ["dev-als"]), "als.wasm")
        }
      }
    | Connection__Download.Channel.Hardcoded => {
        // Web: Check for WASM at hardcoded-als path
        let wasmUri = VSCode.Uri.joinPath(globalStorageUri, ["hardcoded-als", "als.wasm"])
        switch await FS.stat(wasmUri) {
        | Ok(_) => Some(VSCode.Uri.toString(wasmUri))
        | Error(_) => None
        }
      }
    }
  }

  let resolveDownloadChannel = (
    channel: Connection__Download.Channel.t,
    _useCache,
  ) => async (_memento, _globalStorageUri, _platform) => {
    switch channel {
    | LatestALS => {
        // Web doesn't support LatestALS (native binaries)
        Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      }
    | Hardcoded => {
        // Web: Use WASM URL directly
        Ok(
          Connection__Download.Source.FromURL(
            Hardcoded,
            Connection__Hardcoded.wasmUrl,
            "hardcoded-als",
          ),
        )
      }
    | DevALS => {
        // Web only supports Hardcoded channel; DevALS is not available.
        // Runtime clamps Web to Hardcoded, so this branch is unreachable,
        // but we return an error defensively.
        Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      }
    }
  }

  let download = (globalStorageUri, channel, ~trace=Connection__Download__Trace.noop) =>
    Connection__Download.download(globalStorageUri, channel, ~trace)

  // Always download, because there are no alternatives in web environment
  let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.Yes)
}

// Create platform dependencies for web environment
let make = (): Platform.t => module(Web)

// this function is the entry point for the web extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
