module Unzip = Connection__Download__Unzip
module Download = Connection__Download__Util

module Error = {
  type t =
    | ResponseDecodeError(string, Js.Json.t)
    | NoMatchingRelease
    // download
    | AlreadyDownloading
    | CannotDownload(Download.Error.t)
    | CannotGetReleases(Download.Error.t)
    // caching
    | CannotReadReleasesCache(string)
    | CannotCacheReleases(Js.Exn.t)
    // file system
    | CannotChmodFile(string)
    | CannotStatFile(string)
    | CannotReadFile(Js.Exn.t)
    | CannotDeleteFile(string)
    | CannotRenameFile(string)
    // OS
    | CannotDetermineOS(Js.Exn.t)

  let toString = x =>
    switch x {
    | ResponseDecodeError(msg, _) => "Cannot decode release metadata JSON from GitHub:\n" ++ msg
    | NoMatchingRelease => "Cannot find matching release from GitHub"
    // download
    | CannotDownload(error) =>
      "Cannot download file from GitHub:\n" ++ Download.Error.toString(error)
    | CannotGetReleases(error) =>
      "Cannot get release info from GitHub:\n" ++ Download.Error.toString(error)
    | AlreadyDownloading => "Already downloading"
    // caching
    | CannotReadReleasesCache(string) => "Cannot read releases cache:\n" ++ string
    | CannotCacheReleases(exn) => "Failed to cache releases:\n" ++ Util.JsError.toString(exn)
    // file system
    | CannotStatFile(path) => "Cannot stat file \"" ++ path ++ "\""
    | CannotChmodFile(path) => "Cannot chmod file \"" ++ path ++ "\""
    | CannotReadFile(exn) => "Cannot to read files:\n" ++ Util.JsError.toString(exn)
    | CannotDeleteFile(msg) => "Cannot to delete files:\n" ++ msg
    | CannotRenameFile(msg) => "Cannot to rename files:\n" ++ msg
    // OS
    | CannotDetermineOS(exn) => "Cannot determine OS:\n" ++ Util.JsError.toString(exn)
    }
}

module Asset = {
  type t = {
    url: string,
    id: int,
    node_id: string,
    name: string,
    label: string,
    content_type: string,
    state: string,
    size: int,
    created_at: string,
    updated_at: string,
    browser_download_url: string,
  }

  // convert all fields to a JS object-like string
  let toString = asset =>
    "{" ++
    Js.Dict.fromArray([
      ("url", asset.url),
      ("id", string_of_int(asset.id)),
      ("node_id", asset.node_id),
      ("name", asset.name),
      ("label", asset.label),
      ("content_type", asset.content_type),
      ("state", asset.state),
      ("size", string_of_int(asset.size)),
      ("created_at", asset.created_at),
      ("updated_at", asset.updated_at),
      ("browser_download_url", asset.browser_download_url),
    ])
    ->Js.Dict.entries
    ->Array.map(((k, v)) => k ++ ": " ++ v)
    ->Array.join(", ") ++ "}"

  let decode = {
    open JsonCombinators.Json.Decode
    object(field => {
      url: field.required("url", string),
      id: field.required("id", int),
      node_id: field.required("node_id", string),
      name: field.required("name", string),
      label: field.required("label", string),
      content_type: field.required("content_type", string),
      state: field.required("state", string),
      size: field.required("size", int),
      created_at: field.required("created_at", string),
      updated_at: field.required("updated_at", string),
      browser_download_url: field.required("browser_download_url", string),
    })
  }

  let encode = asset => {
    open JsonCombinators.Json.Encode
    Unsafe.object({
      "url": string(asset.url),
      "id": int(asset.id),
      "node_id": string(asset.node_id),
      "name": string(asset.name),
      "label": string(asset.label),
      "content_type": string(asset.content_type),
      "state": string(asset.state),
      "size": int(asset.size),
      "created_at": string(asset.created_at),
      "updated_at": string(asset.updated_at),
      "browser_download_url": string(asset.browser_download_url),
    })
  }

  let chooseByName = (assets: array<t>, name): option<t> => {
    assets->Array.find(asset => asset.name == name)
  }
}

module Release = {
  type t = {
    url: string,
    assets_url: string,
    upload_url: string,
    html_url: string,
    id: int,
    node_id: string,
    tag_name: string,
    target_commitish: string,
    name: string,
    draft: bool,
    prerelease: bool,
    created_at: string,
    published_at: string,
    assets: array<Asset.t>,
    tarball_url: string,
    zipball_url: string,
    body: option<string>,
  }

  // convert all fields to a JS object-like string
  let toString = release =>
    "{" ++
    Js.Dict.fromArray([
      ("url", release.url),
      ("assets_url", release.assets_url),
      ("upload_url", release.upload_url),
      ("html_url", release.html_url),
      ("id", string_of_int(release.id)),
      ("node_id", release.node_id),
      ("tag_name", release.tag_name),
      ("target_commitish", release.target_commitish),
      ("name", release.name),
      ("draft", string_of_bool(release.draft)),
      ("prerelease", string_of_bool(release.prerelease)),
      ("created_at", release.created_at),
      ("published_at", release.published_at),
      ("assets", release.assets->Array.map(Asset.toString)->Array.join(", ")),
      ("tarball_url", release.tarball_url),
      ("zipball_url", release.zipball_url),
      ("body", release.body->Option.map(s => "\"" ++ s ++ "\"")->Option.getOr("null")),
    ])
    ->Js.Dict.entries
    ->Array.map(((k, v)) => k ++ ": " ++ v)
    ->Array.join(", ") ++ "}"

  let decode = {
    open JsonCombinators.Json.Decode
    object(field => {
      url: field.required("url", string),
      assets_url: field.required("assets_url", string),
      upload_url: field.required("upload_url", string),
      html_url: field.required("html_url", string),
      id: field.required("id", int),
      node_id: field.required("node_id", string),
      tag_name: field.required("tag_name", string),
      target_commitish: field.required("target_commitish", string),
      name: field.required("name", string),
      draft: field.required("draft", bool),
      prerelease: field.required("prerelease", bool),
      created_at: field.required("created_at", string),
      published_at: field.required("published_at", string),
      assets: field.required("assets", array(Asset.decode)),
      tarball_url: field.required("tarball_url", string),
      zipball_url: field.required("zipball_url", string),
      body: field.required("body", option(string)),
    })
  }

  let encode = release => {
    open JsonCombinators.Json.Encode
    Unsafe.object({
      "url": string(release.url),
      "assets_url": string(release.assets_url),
      "upload_url": string(release.upload_url),
      "html_url": string(release.html_url),
      "id": int(release.id),
      "node_id": string(release.node_id),
      "tag_name": string(release.tag_name),
      "target_commitish": string(release.target_commitish),
      "name": string(release.name),
      "draft": bool(release.draft),
      "prerelease": bool(release.prerelease),
      "created_at": string(release.created_at),
      "published_at": string(release.published_at),
      "assets": array(Asset.encode)(release.assets),
      "tarball_url": string(release.tarball_url),
      "zipball_url": string(release.zipball_url),
      "body": option(string)(release.body),
    })
  }

  let encodeReleases = releases => {
    open JsonCombinators.Json.Encode
    array(encode)(releases)
  }

  let decodeReleases = json => {
    switch JsonCombinators.Json.decode(json, JsonCombinators.Json.Decode.array(decode)) {
    | Ok(releases) => Ok(releases)
    | Error(e) => Error(Error.ResponseDecodeError(e, json))
    }
  }

  // helper function for selecting the release based on the tag name
  let chooseByTagName = (releases: array<t>, tagName): option<t> => {
    releases->Array.find(release => release.tag_name == tagName)
  }

  // helper function for selecting the latest release
  let chooseLatest = (releases: array<t>): option<t> => {
    // fetch the latest release
    let compare = (x, y) => {
      let xTime = Js.Date.getTime(Js.Date.fromString(x.created_at))
      let yTime = Js.Date.getTime(Js.Date.fromString(y.created_at))
      compare(yTime, xTime)
    }
    let sorted = Js.Array.sortInPlaceWith(compare, releases)
    sorted[0]
  }
}

// Describes which asset to download from the GitHub releases, and what the downloaded file should be named (so that we can cache it)
module FetchSpec = {
  type t = {
    release: Release.t, // the release of the repo
    asset: Asset.t, // the asset of that release
    saveAsFileName: string, // file name of the downloaded asset
  }
}

// helper function for chmoding 744 the executable
let chmodExecutable = async path =>
  switch await NodeJs.Fs.chmod(path, ~mode=0o744) {
  | _ => Ok()
  | exception Exn.Error(_) => Error(Error.CannotChmodFile(path))
  }

module Repo = {
  type t = {
    // GitHub
    username: string,
    repository: string,
    userAgent: string,
    // for caching
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
    cacheInvalidateExpirationSecs: int,
  }

  // convert all fields to a JS object-like string
  let toString = self =>
    "{" ++
    Js.Dict.fromArray([
      ("username", self.username),
      ("repository", self.repository),
      ("userAgent", self.userAgent),
      ("globalStorageUri", self.globalStorageUri->VSCode.Uri.toString),
      ("cacheInvalidateExpirationSecs", string_of_int(self.cacheInvalidateExpirationSecs)),
    ])
    ->Js.Dict.entries
    ->Array.map(((k, v)) => k ++ ": " ++ v)
    ->Array.join(", ") ++ "}"
}

module Callbacks = {
  type t = {
    chooseFromReleases: array<Release.t> => option<FetchSpec.t>,
    onDownload: Download.Event.t => unit,
    log: string => unit,
  }
}

module ReleaseManifest: {
  // age of the release manifest cache in seconds
  let cacheAgeInSecs: Memento.t => option<int>
  // fetch the release manifest from the cache or GitHub
  let fetch: Repo.t => promise<(result<array<Release.t>, Error.t>, bool)>
  // fresh fetch from GitHub and cache it
  let fetchFromGitHubAndCache: Repo.t => promise<result<array<Release.t>, Error.t>>
} = {
  // timestamp for the release cache
  let readTimestamp = memento =>
    memento->Memento.get("alsReleaseCacheTimestamp")->Option.map(Date.fromString)
  let writeTimestamp = (memento, timestamp) =>
    memento->Memento.set("alsReleaseCacheTimestamp", Date.toString(timestamp))

  // release cache
  let readReleaseCache = memento => memento->Memento.get("alsReleaseCache")
  let writeReleaseCache = (memento, releases) =>
    memento->Memento.set("alsReleaseCache", releases)

  // return the time difference in seconds since the cache was last fetched
  let cacheAgeInSecs = memento => {
    switch readTimestamp(memento) {
    | None => None
    | Some(timestamp) =>
      let currentTime = Date.now()
      let lastModifiedTime = Date.getTime(timestamp)
      // time difference in seconds
      Some(int_of_float((currentTime -. lastModifiedTime) /. 1000.0))
    }
  }

  let fetchFromCache = async memento => {
    // read the file and decode as json
    switch readReleaseCache(memento) {
    | None => Ok([])
    | Some(string) =>
      // parse the json
      switch Js.Json.parseExn(string) {
      | json =>
        switch Release.decodeReleases(json) {
        | Error(e) => Error(e)
        | Ok(releases) => Ok(releases)
        }
      | exception _ => Error(Error.CannotReadReleasesCache(string))
      }
    }
  }

  let writeToCache = async (memento, releases) => {
    let json = Release.encodeReleases(releases)->Js_json.stringify
    await writeTimestamp(memento, Date.make())
    await writeReleaseCache(memento, json)
  }

  // fetch the latest release from GitHub and cache it
  // timeouts after 10000ms
  let fetchFromGitHubAndCache = async (repo: Repo.t) => {
    let httpOptions = {
      "host": "api.github.com",
      "path": "/repos/" ++ repo.username ++ "/" ++ repo.repository ++ "/releases",
      "headers": {
        "User-Agent": repo.userAgent,
      },
    }
    switch await Download.asJson(httpOptions)->Download.timeoutAfter(10000) {
    | Error(e) => Error(Error.CannotGetReleases(e))
    | Ok(json) =>
      switch Release.decodeReleases(json) {
      | Error(e) => Error(e)
      | Ok(releases) =>
        await writeToCache(repo.memento, releases)
        Ok(releases)
      }
    }
  }

  // fetch from GitHub if the cache is too old
  // also returns a boolean indicating if the result is from cache
  let fetch = async (repo: Repo.t) => {
    // let cacheAge = cacheAgeInSecs(repo.memento)

    let cacheInvalidated = switch cacheAgeInSecs(repo.memento) {
    | None => true
    | Some(cacheAge) => cacheAge > repo.cacheInvalidateExpirationSecs
    }

    if cacheInvalidated {
      (await fetchFromGitHubAndCache(repo), false)
    } else {
      (await fetchFromCache(repo.memento), true)
    }
  }
}

module Module: {
  let download: (
    FetchSpec.t,
    Memento.t,
    VSCode.Uri.t,
    Download.Event.t => unit,
  ) => promise<result<bool, Error.t>>
  let isDownloading: VSCode.Uri.t => promise<bool>
} = {
  let inFlightDownloadFileName = "in-flight.download"

  // in-flight download will be named as "in-flight.download"
  // see if "in-flight.download" already exists
  let isDownloading = async globalStorageUri => {
    try {
      let exists = switch await FS.stat(globalStorageUri) {
      | Ok(_) => true
      | Error(_) => false
      }

      if exists {
        let inFlightDownloadUri = VSCode.Uri.joinPath(globalStorageUri, [inFlightDownloadFileName])
        switch await FS.stat(inFlightDownloadUri) {
        | Ok(_) => true  // File exists, download is in progress
        | Error(_) => false  // File doesn't exist, no download in progress
        }
      } else {
        // create a directory for `context.globalStoragePath` if it doesn't exist
        let _ = await FS.createDirectory(globalStorageUri)
        false
      }
    } catch {
    | _ => false
    }
  }

  let downloadLanguageServer = async (repo: Repo.t, onDownload, fetchSpec: FetchSpec.t) => {
    let url = NodeJs.Url.make(fetchSpec.asset.browser_download_url)
    let httpOptions = {
      "host": url.host,
      "path": url.pathname,
      "headers": {
        "User-Agent": repo.userAgent,
      },
    }

    let inFlightDownloadUri = VSCode.Uri.joinPath(repo.globalStorageUri, [inFlightDownloadFileName])
    let inFlightDownloadZipUri = VSCode.Uri.joinPath(inFlightDownloadUri, [".zip"])
    let destPath = VSCode.Uri.joinPath(repo.globalStorageUri, [fetchSpec.saveAsFileName])

    let result = switch await Download.asFile(httpOptions, inFlightDownloadUri, onDownload) {
    | Error(e) => Error(Error.CannotDownload(e))
    | Ok() =>
      // suffix with ".zip" after downloaded
      switch await FS.rename(inFlightDownloadUri, inFlightDownloadZipUri) {
      | Error(e) => Error(Error.CannotRenameFile(e))
      | Ok() =>
        // unzip the downloaded file
        await Unzip.run(inFlightDownloadZipUri, destPath)
        // remove the zip file
        switch await FS.delete(inFlightDownloadZipUri) {
        | Error(e) => Error(Error.CannotDeleteFile(e))
        | Ok() => Ok()
        }
      }
    }

    // cleanup on error
    switch result {
    | Error(error) =>
      let delete = async uri => {
        let _ = await FS.delete(uri)
      }

      let _ = await Promise.all([delete(inFlightDownloadUri), delete(inFlightDownloadZipUri)])
      Error(error)
    | Ok() => Ok()
    }
  }

  let download = async (fetchSpec: FetchSpec.t, memento, globalStorageUri, reportProgress) => {
    let repo: Repo.t = {
      username: "agda",
      repository: "agda-language-server",
      userAgent: "agda/agda-mode-vscode",
      memento,
      globalStorageUri,
      cacheInvalidateExpirationSecs: 86400,
    }

    let ifIsDownloading = await isDownloading(repo.globalStorageUri)
    if ifIsDownloading {
      Error(Error.AlreadyDownloading)
    } else {
      // don't download from GitHub if `fetchSpec.fileName` already exists
      let destUri = VSCode.Uri.joinPath(repo.globalStorageUri, [fetchSpec.saveAsFileName])
      switch await FS.stat(destUri) {
      | Ok(_) => Ok(true)
      | Error(_) =>
        switch await downloadLanguageServer(repo, reportProgress, fetchSpec) {
        | Error(error) => Error(error)
        | Ok() =>
          // chmod the executable after download
          // (no need to chmod if it's on Windows)
          let destPath = VSCode.Uri.fsPath(destUri)
          let execPath = NodeJs.Path.join2(destPath, "als")
          let shouldChmod = OS.onUnix
          if shouldChmod {
            let _ = await chmodExecutable(execPath)
          }
          Ok(false)
        }
      }
    }
  }
}

include Module
