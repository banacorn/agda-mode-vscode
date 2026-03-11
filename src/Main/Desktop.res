// Desktop-specific entry point that creates Platform.makeDesktop() dependencies
// This file will be used for the desktop bundle

// Desktop implementation - uses actual platform-specific code
module Desktop: Platform.PlatformOps = {
  let determinePlatform = Connection__Download__Platform.determine

  let findCommand = Connection__Command.search

  let alreadyDownloaded = async (globalStorageUri, channel) => {
    switch channel {
    | Connection__Download.Channel.LatestALS => {
        let uri = VSCode.Uri.joinPath(globalStorageUri, ["latest-als", "als"])
        switch await FS.stat(uri) {
        | Ok(_) =>
          Util.log("[ debug ] alreadyDownloaded LatestALS: found", uri->VSCode.Uri.fsPath)
          Some(uri->VSCode.Uri.fsPath)
        | Error(_) =>
          Util.log("[ debug ] alreadyDownloaded LatestALS: not found", uri->VSCode.Uri.fsPath)
          None
        }
      }
    | Connection__Download.Channel.DevALS => {
        // Desktop: Only check for native binary (als or als.exe), not WASM
        let alsUri = VSCode.Uri.joinPath(globalStorageUri, ["dev-als", "als"])
        switch await FS.stat(alsUri) {
        | Ok(_) =>
          Util.log("[ debug ] alreadyDownloaded DevALS: found", alsUri->VSCode.Uri.fsPath)
          Some(alsUri->VSCode.Uri.fsPath)
        | Error(_) =>
          Util.log("[ debug ] alreadyDownloaded DevALS: not found", alsUri->VSCode.Uri.fsPath)
          None
        }
      }
    | Connection__Download.Channel.Hardcoded => {
        // Desktop: Prefer native, then fall back to WASM cache.
        let alsUri = VSCode.Uri.joinPath(globalStorageUri, ["hardcoded-als", "als"])
        switch await FS.stat(alsUri) {
        | Ok(_) =>
          let alsPath = alsUri->VSCode.Uri.fsPath
          Util.log("[ debug ] alreadyDownloaded Hardcoded: native found", alsPath)
          if OS.onUnix {
            let _ = await NodeJs.Fs.chmod(alsPath, ~mode=0o744)
          }
          Some(alsPath)
        | Error(_) =>
          Util.log("[ debug ] alreadyDownloaded Hardcoded: native not found", alsUri->VSCode.Uri.fsPath)
          let wasmUri = VSCode.Uri.joinPath(globalStorageUri, ["hardcoded-als", "als.wasm"])
          switch await FS.stat(wasmUri) {
          | Ok(_) =>
            Util.log("[ debug ] alreadyDownloaded Hardcoded: wasm found", wasmUri->VSCode.Uri.toString)
            Some(VSCode.Uri.toString(wasmUri))
          | Error(_) =>
            Util.log("[ debug ] alreadyDownloaded Hardcoded: nothing found", "")
            None
          }
        }
      }
    }
  }

  let resolveDownloadChannel = (
    channel: Connection__Download.Channel.t,
    useCache,
  ) => async (memento, globalStorageUri, platform) => {
    switch channel {
    | Hardcoded =>
      switch Connection__Hardcoded.nativeUrlForPlatform(platform) {
      | Some(url) =>
        Ok(Connection__Download.Source.FromURL(Hardcoded, url, "hardcoded-als"))
      | None =>
        Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      }
    | LatestALS | DevALS =>
      let repo = switch channel {
      | LatestALS => Connection__LatestALS.makeRepo(globalStorageUri)
      | DevALS => Connection__DevALS.makeRepo(globalStorageUri)
      | Hardcoded => Connection__LatestALS.makeRepo(globalStorageUri) // unreachable
      }

      let toDownloadOrder = switch channel {
      | LatestALS => Connection__LatestALS.toDownloadOrder(_, platform)
      | DevALS => Connection__DevALS.toDownloadOrder(_, platform)
      | Hardcoded => Connection__LatestALS.toDownloadOrder(_, platform) // unreachable
      }

      switch await Connection__Download.getReleaseManifestFromGitHub(memento, repo, ~useCache) {
      | Error(error) => Error(error)
      | Ok(releases) => toDownloadOrder(releases)
      }
    }
  }
  let download = Connection__Download.download

  let askUserAboutDownloadPolicy = async () => {
    let messageOptions = {
      VSCode.MessageOptions.modal: true,
      detail: "Do you want to download and install the latest Agda Language Server?",
    }
    let result = await VSCode.Window.showWarningMessageWithOptions(
      "Cannot find Agda or Agda Language Server",
      messageOptions,
      [
        Config.Connection.DownloadPolicy.toString(Yes),
        Config.Connection.DownloadPolicy.toString(No),
      ],
    ) // 📺

    // parse the result
    result->Option.mapOr(
      Config.Connection.DownloadPolicy.No,
      Config.Connection.DownloadPolicy.fromString,
    )
  }
}

// Create platform dependencies for desktop environment
let make = (): Platform.t => module(Desktop)

// this function is the entry point for the desktop extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
