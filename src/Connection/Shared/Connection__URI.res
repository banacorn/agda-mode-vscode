@module external untildify: string => string = "untildify"

type t = FileURI(VSCode.Uri.t) | LspURI(NodeJs.Url.t)

// Trying to parse a raw path as a URI with lsp: scheme or as a file path.
// Although file paths are stored using VSCode.Uri, it
//    * does not resolve absolute paths with drive letters like "c:/" on Windows.
//    * does not expanded paths with "~/" to the user's home directory.
//    * does not normalize paths with ".." segments.
// So we'll have to handle these cases before converting to VSCode.Uri.
let parse = path => {
  let result = try Some(NodeJs.Url.make(path)) catch {
  | _ => None
  }
  // single out the URL with the protocol "lsp:"
  let result = switch result {
  | Some(url) =>
    if url.protocol == "lsp:" {
      Some(url)
    } else {
      None
    }
  | None => None
  }
  // only treat URLs with the protocol "lsp:" as URLs
  switch result {
  | Some(url) => LspURI(url)
  | None =>
    // normalize the path by replacing the tilde "~/" with the absolute path of home directory
    let path = untildify(path)
    let path = NodeJs.Path.normalize(path)

    // on Windows, paths that start with a drive letter like "/c/path/to/agda" will be converted to "c:/path/to/agda"
    let path = if OS.onUnix {
      path
    } else {
      path->String.replaceRegExp(%re("/^\\([a-zA-Z])\\/"), "$1\:\\")
    }

    let absolutePath = NodeJs.Path.resolve([path])

    FileURI(VSCode.Uri.file(absolutePath))
  }
}

let toVSCodeURI = uri =>
  switch uri {
  | FileURI(uri) => Some(uri)
  | LspURI(_) => None
  }

let toString = uri =>
  switch uri {
  | FileURI(vscodeUri) => VSCode.Uri.toString(vscodeUri)
  | LspURI(nodeJsUrl) => nodeJsUrl.toString()
  }

let equal = (x, y) =>
  switch (x, y) {
  | (FileURI(x), FileURI(y)) => VSCode.Uri.toString(x) == VSCode.Uri.toString(y)
  | (LspURI(x), LspURI(y)) => x.toString() == y.toString()
  | _ => false
  }
