@module external untildify: string => string = "untildify"

type t = Filepath(string) | URL(NodeJs.Url.t)

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
  | Some(url) => URL(url)
  | None =>
    // normailize the path by replacing the tild "~/" with the absolute path of home directory
    let path = untildify(path)
    let path = NodeJs.Path.normalize(path)

    // on Windows, paths that start with a drive letter like "/c/path/to/agda" will be converted to "c:/path/to/agda"
    let path = if OS.onUnix {
      path
    } else {
      path->String.replaceRegExp(%re("/^\\([a-zA-Z])\\/"), "$1\:\\")
    }
    Filepath(path)
  }
}

let toString = path =>
  switch path {
  | Filepath(path) => NodeJs.Path.normalize(path)
  | URL(url) => url.toString()
  }
