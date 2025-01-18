// like Source.t but with more info for display
type source =
  | FromFile(string) // path of the program
  | FromCommand(string) // name of the command
  | FromTCP(NodeJs.Url.t)
  | FromGitHub(
      Connection__Resolver__GitHub.Repo.t,
      Connection__Resolver__GitHub.Release.t,
      Connection__Resolver__GitHub.Asset.t,
    )

let sourceToString = source =>
  switch source {
  | FromFile(path) => "File: " ++ path
  | FromCommand(name) => "Command: " ++ name
  | FromTCP(url) => "TCP: " ++ url.toString()
  | FromGitHub(repo, release, asset) =>
    "GitHub: " ++
    Connection__Resolver__GitHub.Repo.toString(repo) ++
    " " ++
    Connection__Resolver__GitHub.Release.toString(release) ++
    " " ++
    Connection__Resolver__GitHub.Asset.toString(asset)
  }

// Means of Inter-process communication
type t =
  | ViaPipe(
      string,
      array<string>,
      option<Connection__Target__ALS__LSP__Binding.executableOptions>,
      source,
    ) // command, args, options, source
  | ViaTCP(NodeJs.Url.t, source)
