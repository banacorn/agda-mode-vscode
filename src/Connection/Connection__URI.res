@module external untildify: string => string = "untildify"

type t = Filepath(string) | URL(NodeJs.Url.t)

// trying to parse a raw path as a URL or else a file path
let parse = async path => {
  let result = try Some(NodeJs.Url.make(path)) catch {
  | _ => None
  }
  switch result {
  | Some(url) => URL(url)
  | None =>
    // treat the path as a file path
    let path = untildify(path)
    let path = NodeJs.Path.normalize(path)
    Filepath(path)
  }
}

let toString = path =>
  switch path {
  | Filepath(path) => path
  | URL(url) => url.toString()
  }
