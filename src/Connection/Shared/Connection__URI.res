// External binding to the untildify npm package for expanding tilde (~) in file paths.
// Converts paths like "~/Documents" to "/Users/username/Documents".
@module external untildify: string => string = "untildify"

// URI type representing paths wrapped in VSCode.Uri.t.
// Accepted inputs: Filepath, bare command, file:// URI, and vscode-* URI schemes.
type raw = string
type t = FileURI(raw, VSCode.Uri.t)

let startsWithWindowsDriveAfterSlash = (path: string): bool =>
  if String.length(path) < 3 {
    false
  } else {
    switch (String.charAt(path, 0), String.charAt(path, 1), String.charAt(path, 2)) {
    | ("/", letter, "/") =>
      let code = String.charCodeAt(letter, 0)
      (code >= 65. && code <= 90.) || (code >= 97. && code <= 122.)
    | _ => false
    }
  }

let normalizeToBackslashes = (path: string): string => {
  let rec loop = (index: int, out: string): string =>
    if index >= String.length(path) {
      out
    } else {
      let ch = String.charAt(path, index)
      let out = if ch == "/" { out ++ "\\" } else { out ++ ch }
      loop(index + 1, out)
    }
  loop(0, "")
}

let windowsSlashAbsoluteToDrivePath = (path: string): option<string> =>
  if startsWithWindowsDriveAfterSlash(path) {
    let driveLetter = String.charAt(path, 1)
    let rest = String.sliceToEnd(path, ~start=2)
    let windowsRest = normalizeToBackslashes(rest)
    Some((driveLetter ++ ":" ++ windowsRest)->NodeJs.Path.normalize)
  } else {
    None
  }

// Parses a raw string into a Connection URI, handling various path formats.
//
// The function performs intelligent parsing by:
// 1. Preserving vscode-* URI schemes (vscode-userdata:, vscode-vfs:, etc.)
//    using VSCode.Uri.parse to keep the scheme intact — needed on web where
//    downloaded WASM paths use non-file:// schemes
// 2. Extracting file paths from file:// URLs
// 3. Processing file paths through normalization pipeline:
//    - Tilde expansion (~/ -> /Users/username/)
//    - Path normalization (removing .. and . segments)
//    - Windows drive letter conversion (/c/path -> c:\path)
//    - Relative to absolute path resolution
// 4. Creating VSCode.Uri for file paths
//
// All other URI schemes (lsp://, http://, etc.) are treated as file paths.
//
// Examples:
// parse("relative/path.txt") // -> FileURI with absolute path
// parse("~/Documents/file.txt") // -> FileURI with expanded home directory
// parse("/usr/bin/../bin/agda") // -> FileURI with normalized path
// parse("file:///absolute/path") // -> FileURI with extracted file path
// parse("vscode-userdata:/path/als.wasm") // -> FileURI with preserved scheme
let parse = raw => {
  // Preserve vscode-* URI schemes (used on web for global storage paths)
  // These must NOT be coerced to file:// — use VSCode.Uri.parse to keep the scheme
  if String.startsWith(raw, "vscode-") {
    FileURI(raw, VSCode.Uri.parse(raw))
  } else {
    // Extract file path from file:// URLs
    let filePath = if String.startsWith(raw, "file://") {
      // Convert file:// URL to file path using VSCode.Uri
      VSCode.Uri.parse(raw)->VSCode.Uri.fsPath
    } else {
      raw
    }
    // normalize the path by replacing the tilde "~/" with the absolute path of home directory
    let path = untildify(filePath)
    let absolutePath = if OS.onUnix {
      let path = NodeJs.Path.normalize(path)
      NodeJs.Path.resolve([path])
    } else {
      switch windowsSlashAbsoluteToDrivePath(path) {
      | Some(windowsDrivePath) => NodeJs.Path.resolve([windowsDrivePath])
      | None =>
        let path = NodeJs.Path.normalize(path)
        NodeJs.Path.resolve([path])
      }
    }

    FileURI(raw, VSCode.Uri.file(absolutePath))
  }
}

// Converts a Connection URI back to its string representation.
//
// Returns VSCode.Uri.toString() format (typically file:// URLs).
//
// Note: The output may not be identical to the original input due to normalization.
// For example, relative paths become absolute, tildes are expanded, etc.
//
// Examples:
// let uri = parse("relative/path.txt")
// toString(uri) // -> "file:///absolute/path/to/relative/path.txt"
let toString = uri =>
  switch uri {
  | FileURI(_, vscodeUri) => VSCode.Uri.toString(vscodeUri)
  }

// Checks if two Connection URIs are equal by comparing their string representations.
//
// This approach handles VSCode.Uri object identity issues by comparing the normalized
// string representations instead of the objects directly.
//
// Examples:
// let uri1 = parse("/usr/bin/agda")
// let uri2 = parse("/usr/bin/agda")
// equal(uri1, uri2) // -> true
let equal = (x, y) =>
  switch (x, y) {
  | (FileURI(_, x), FileURI(_, y)) => VSCode.Uri.toString(x) == VSCode.Uri.toString(y)
  }
