// Exception-safe wrappers around VS Code FileSystem API

let readDirectory = async (uri: VSCode.Uri.t): result<array<(string, VSCode.FileType.t)>, string> => {
  try {
    let entries = await VSCode.Workspace.fs->VSCode.FileSystem.readDirectory(uri)
    Ok(entries)
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}