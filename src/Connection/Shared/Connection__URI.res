// External binding to the untildify npm package for expanding tilde (~) in file paths.
// Converts paths like "~/Documents" to "/Users/username/Documents".
@module external untildify: string => string = "untildify"

// Union type representing different URI schemes supported by the Connection module.
// - FileURI: Local file paths wrapped in VSCode.Uri.t
// - LspURI: Language Server Protocol URLs wrapped in URL.t
type raw = string
type t = FileURI(raw, VSCode.Uri.t) | LspURI(raw, URL.t)

// Parses a raw string into a Connection URI, handling various path formats and URL schemes.
//
// The function performs intelligent parsing by:
// 1. First attempting to parse as a URL (lsp:// or file://)
// 2. Handling lsp:// URLs directly as LspURI
// 3. Converting file:// URLs back to file paths using VSCode.Uri
// 4. Processing file paths through normalization pipeline:
//    - Tilde expansion (~/ -> /Users/username/)
//    - Path normalization (removing .. and . segments)
//    - Windows drive letter conversion (/c/path -> c:\path)
//    - Relative to absolute path resolution
// 5. Creating VSCode.Uri for file paths
//
// Examples:
// parse("relative/path.txt") // -> FileURI with absolute path
// parse("~/Documents/file.txt") // -> FileURI with expanded home directory
// parse("/usr/bin/../bin/agda") // -> FileURI with normalized path
// parse("file:///absolute/path") // -> FileURI with extracted file path
// parse("lsp://localhost:8080") // -> LspURI
let parse = raw => {
  let result = try Some(URL.make(raw)) catch {
  | _ => None
  }
  // Handle URLs with specific protocols
  let result = switch result {
  | Some(url) =>
    if url->URL.protocol == "lsp:" {
      Some(url)
    } else if url->URL.protocol == "file:" {
      // Convert file:// URL back to file path
      None // This will fall through to file path handling
    } else {
      None
    }
  | None => None
  }
  // Handle LSP URLs or fall back to file path processing
  switch result {
  | Some(url) => LspURI(raw, url)
  | None =>
    // Extract file path from file:// URLs
    let filePath = if String.startsWith(raw, "file://") {
      // Convert file:// URL to file path using VSCode.Uri
      VSCode.Uri.parse(raw)->VSCode.Uri.fsPath
    } else {
      raw
    }
    // normalize the path by replacing the tilde "~/" with the absolute path of home directory
    let path = untildify(filePath)
    let path = NodeJs.Path.normalize(path)

    // on Windows, paths that start with a drive letter like "/c/path/to/agda" will be converted to "c:/path/to/agda"
    let path = if OS.onUnix {
      path
    } else {
      path->String.replaceRegExp(%re("/^\\([a-zA-Z])\\/"), "$1\:\\")
    }

    let absolutePath = NodeJs.Path.resolve([path])

    FileURI(raw, VSCode.Uri.file(absolutePath))
  }
}

// // Extracts the VSCode.Uri from a FileURI variant, if possible.
// // This is useful when you need to work with VSCode APIs that expect VSCode.Uri.t directly.
// //
// // Returns Some(VSCode.Uri.t) for FileURI variants, None for LspURI variants.
// //
// // Examples:
// // let fileUri = parse("path/to/file.txt")
// // let vscodeUri = toVSCodeURI(fileUri) // -> Some(VSCode.Uri.t)
// //
// // let lspUri = parse("lsp://localhost:8080")
// // let vscodeUri = toVSCodeURI(lspUri) // -> None
// let toVSCodeURI = uri =>
//   switch uri {
//   | FileURI(_, uri) => Some(uri)
//   | LspURI(_) => None
//   }

// Converts a Connection URI back to its string representation.
//
// The output format depends on the URI variant:
// - FileURI: Returns VSCode.Uri.toString() format (typically file:// URLs)
// - LspURI: Returns the original URL string format
//
// Note: The output may not be identical to the original input due to normalization.
// For example, relative paths become absolute, tildes are expanded, etc.
//
// Examples:
// let uri = parse("relative/path.txt")
// toString(uri) // -> "file:///absolute/path/to/relative/path.txt"
//
// let lspUri = parse("lsp://localhost:8080")
// toString(lspUri) // -> "lsp://localhost:8080"
let toString = uri =>
  switch uri {
  | FileURI(_, vscodeUri) => VSCode.Uri.toString(vscodeUri)
  | LspURI(_, url) => url->URL.toString
  }

// Checks if two Connection URIs are equal by comparing their string representations.
//
// Equality is determined by:
// - FileURI vs FileURI: Compare VSCode.Uri.toString() outputs
// - LspURI vs LspURI: Compare NodeJs.Url.toString() outputs
// - Different variants: Always false
//
// This approach handles VSCode.Uri object identity issues by comparing the normalized
// string representations instead of the objects directly.
//
// Examples:
// let uri1 = parse("/usr/bin/agda")
// let uri2 = parse("/usr/bin/agda")
// equal(uri1, uri2) // -> true
//
// let uri3 = parse("lsp://localhost:8080")
// equal(uri1, uri3) // -> false (different variants)
let equal = (x, y) =>
  switch (x, y) {
  | (FileURI(_, x), FileURI(_, y)) => VSCode.Uri.toString(x) == VSCode.Uri.toString(y)
  | (LspURI(_, x), LspURI(_, y)) => x->URL.toString == y->URL.toString
  | _ => false
  }
