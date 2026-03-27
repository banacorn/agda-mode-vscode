type t =
  | Command(string)
  | Resource(VSCode.Uri.t)

module Resolved = {
  type t = {
    original: Candidate.t,
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
    let hasWhitespace = switch String.match(command, %re("/\\s/")) {
    | Some(_) => true
    | None => false
    }
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

let isUnderDirectory = (candidate: t, directory: VSCode.Uri.t): bool =>
  switch candidate {
  | Command(_) => false
  | Resource(resource) =>
    if
      VSCode.Uri.scheme(resource) != VSCode.Uri.scheme(directory) ||
        VSCode.Uri.authority(resource) != VSCode.Uri.authority(directory)
    {
      false
    } else if VSCode.Uri.scheme(resource) == "file" {
      let resourceFsPath = VSCode.Uri.fsPath(resource)
      let directoryFsPath = VSCode.Uri.fsPath(directory)
      isUnderPrefix(resourceFsPath, directoryFsPath, NodeJs.Path.sep) ||
      isUnderPrefix(resourceFsPath, directoryFsPath, "/")
    } else {
      let resourcePath = VSCode.Uri.path(resource)
      let directoryPath = VSCode.Uri.path(directory)
      isUnderPrefix(resourcePath, directoryPath, "/")
    }
  }

let resolve = async (platformDeps: Platform.t, candidate: t): result<
  Resolved.t,
  Connection__Command.Error.t,
> => {
  module PlatformOps = unpack(platformDeps)
  switch candidate {
  | Command(command) =>
    switch await PlatformOps.findCommand(command) {
    | Ok(path) =>
      switch Connection__URI.parse(path) {
      | FileURI(_, uri) => Ok({original: candidate, resource: uri})
      }
    | Error(error) => Error(error)
    }
  | Resource(uri) => Ok({original: candidate, resource: uri})
  }
}
