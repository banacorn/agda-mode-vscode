type candidate =
  | Command(string)
  | Resource(VSCode.Uri.t)

type t = candidate

module Resolved = {
  type t = {
    original: candidate,
    resource: VSCode.Uri.t,
  }

  let toString = ({original, resource}) =>
    (
      switch original {
      | Command(command) => command
      | Resource(uri) => uri->VSCode.Uri.toString
      }
    ) ++ " => " ++ resource->VSCode.Uri.toString
}

let isCommand = raw => {
  let command = raw->String.trim
  if command == "" {
    false
  } else {
    let hasWhitespace = command != (command->String.trim)
      || String.includes(command, " ")
      || String.includes(command, "\t")
      || String.includes(command, "\n")
      || String.includes(command, "\r")
    let hasScheme = switch String.match(command, %re("/^[a-zA-Z][a-zA-Z0-9+.-]*:/")) {
    | Some(_) => true
    | None => false
    }
    let hasSeparator = String.includes(command, "/") || String.includes(command, "\\")
    let startsLikePath = String.startsWith(command, ".") || String.startsWith(command, "~")
    let isAbsolute = NodeJs.Path.isAbsolute(command)

    !(hasWhitespace || hasScheme || hasSeparator || startsLikePath || isAbsolute)
  }
}

let make = raw => {
  let raw = raw->String.trim
  if isCommand(raw) {
    Command(raw)
  } else {
    switch Connection__URI.parse(raw) {
    | FileURI(_, uri) => Resource(uri)
    }
  }
}

let toString = candidate =>
  switch candidate {
  | Command(command) => command
  | Resource(uri) => uri->VSCode.Uri.toString
  }

let equal = (x, y) =>
  switch (x, y) {
  | (Command(x), Command(y)) => x == y
  | (Resource(x), Resource(y)) => VSCode.Uri.toString(x) == VSCode.Uri.toString(y)
  | _ => false
  }

let deduplicate = candidates => {
  let seen = ref([])
  candidates->Array.filter(candidate => {
    let alreadySeen = seen.contents->Array.some(existing => equal(existing, candidate))
    if !alreadySeen {
      seen := Array.concat(seen.contents, [candidate])
    }
    !alreadySeen
  })
}

let isUnderPrefix = (path: string, prefix: string, separator: string): bool =>
  path == prefix || String.startsWith(path, prefix ++ separator)

let rec stripTrailingSlashes = (path: string): string =>
  if path == "/" {
    path
  } else if String.endsWith(path, "/") {
    stripTrailingSlashes(
      String.slice(path, ~start=0, ~end=String.length(path) - 1),
    )
  } else {
    path
  }

let normalizeWindowsSlashes = (path: string): string => {
  let rec loop = (index: int, out: string): string =>
    if index >= String.length(path) {
      out
    } else {
      let ch = String.charAt(path, index)
      let out = if ch == "\\" { out ++ "/" } else { out ++ ch }
      loop(index + 1, out)
    }
  loop(0, "")
}

let startsWithWindowsDriveAfterSlash = (path: string): bool =>
  if String.length(path) < 4 {
    false
  } else {
    switch (String.charAt(path, 0), String.charAt(path, 1), String.charAt(path, 2), String.charAt(path, 3)) {
    | ("/", letter, ":", "/") =>
      String.charCodeAt(letter, 0) >= 97. &&
      String.charCodeAt(letter, 0) <= 122.
    | _ => false
    }
  }

let normalizeComparableLocalPath = (path: string): string =>
  if OS.onUnix {
    stripTrailingSlashes(path)
  } else {
    path
    ->normalizeWindowsSlashes
    ->String.toLowerCase
    ->(path => startsWithWindowsDriveAfterSlash(path) ? String.sliceToEnd(path, ~start=1) : path)
    ->stripTrailingSlashes
  }

let isUnderComparableLocalPath = (~path: string, ~prefix: string): bool =>
  isUnderPrefix(
    normalizeComparableLocalPath(path),
    normalizeComparableLocalPath(prefix),
    "/",
  )

let localComparablePath = (uri: VSCode.Uri.t): option<string> =>
  switch (VSCode.Uri.scheme(uri), VSCode.Uri.authority(uri)) {
  | ("file", "") => Some(VSCode.Uri.fsPath(uri))
  | ("vscode-userdata", "") => Some(Js.Global.decodeURIComponent(VSCode.Uri.path(uri)))
  | _ => None
  }

let isUnderDirectory = (candidate: t, directory: VSCode.Uri.t): bool =>
  switch candidate {
  | Command(_) => false
  | Resource(resource) =>
    if VSCode.Uri.scheme(resource) == VSCode.Uri.scheme(directory) &&
        VSCode.Uri.authority(resource) == VSCode.Uri.authority(directory) {
      if VSCode.Uri.scheme(resource) == "file" {
        let resourceFsPath = VSCode.Uri.fsPath(resource)
        let directoryFsPath = VSCode.Uri.fsPath(directory)
        isUnderComparableLocalPath(~path=resourceFsPath, ~prefix=directoryFsPath)
      } else {
        let resourcePath = VSCode.Uri.path(resource)
        let directoryPath = VSCode.Uri.path(directory)
        isUnderPrefix(resourcePath, directoryPath, "/")
      }
    } else if VSCode.Uri.scheme(resource) == "file" {
      switch (localComparablePath(resource), localComparablePath(directory)) {
      | (Some(resourcePath), Some(directoryPath)) =>
        isUnderComparableLocalPath(~path=resourcePath, ~prefix=directoryPath)
      | _ => false
      }
    } else {
      switch (localComparablePath(resource), localComparablePath(directory)) {
      | (Some(resourcePath), Some(directoryPath)) =>
        isUnderComparableLocalPath(~path=resourcePath, ~prefix=directoryPath)
      | _ => false
      }
    }
  }

let resolve: (
  (string, ~timeout: int=?) => promise<result<string, Connection__Command.Error.t>>,
  t,
) => promise<result<Resolved.t, Connection__Command.Error.t>> = async (findCommand, candidate) => {
  switch candidate {
  | Command(command) =>
    switch await findCommand(command) {
    | Ok(path) =>
      switch Connection__URI.parse(path) {
      | FileURI(_, uri) =>
        let resolved: Resolved.t = {original: candidate, resource: uri}
        Ok(resolved)
      }
    | Error(error) => Error(error)
    }
  | Resource(uri) =>
    let resolved: Resolved.t = {original: candidate, resource: uri}
    Ok(resolved)
  }
}
