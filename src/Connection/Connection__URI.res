@module external untildify: string => string = "untildify"

type t = Filepath(string) | URL(NodeJs.Url.t)
let parse = async path => {
  // trying to parse the path as a URL
  let result = try Some(NodeJs.Url.make(path)) catch {
  | _ => None
  }
  switch result {
  | Some(url) => Some(URL(url))
  | None =>
    // treat the path as a file path
    let path = untildify(path)
    let path = NodeJs.Path.normalize(path)
    Some(Filepath(path))
  }
}

let toString = path =>
  switch path {
  | Filepath(path) => path
  | URL(url) => url.toString()
  }
