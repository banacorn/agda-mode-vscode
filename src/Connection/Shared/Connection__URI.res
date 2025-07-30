@module external untildify: string => string = "untildify"

type t = FileURI(VSCode.Uri.t) | LspURI(NodeJs.Url.t)

// trying to parse a raw path as a URL or else a file path
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
    
    // Convert relative paths to absolute paths before creating VSCode URI
    let absolutePath = if NodeJs.Path.isAbsolute(path) {
      path
    } else {
      NodeJs.Path.resolve([path])
    }
    
    FileURI(VSCode.Uri.file(absolutePath))
  }
}

let toString = uri =>
  switch uri {
  | FileURI(vscodeUri) => NodeJs.Path.normalize(VSCode.Uri.fsPath(vscodeUri))
  | LspURI(nodeJsUrl) => nodeJsUrl.toString()
  }

let equal = (x, y) =>
  switch (x, y) {
  | (FileURI(x), FileURI(y)) => VSCode.Uri.toString(x) == VSCode.Uri.toString(y)
  | (LspURI(x), LspURI(y)) => x.toString() == y.toString()
  | _ => false
  }
