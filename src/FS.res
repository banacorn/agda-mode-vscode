// Exception-safe interface to VSCode.FileSystem
// Returns result types instead of throwing exceptions

let copy = async (source: VSCode.Uri.t, destination: VSCode.Uri.t): result<unit, string> => {
  try {
    await VSCode.Workspace.fs->VSCode.FileSystem.copy(source, destination)
    Ok()
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}

let createDirectory = async (uri: VSCode.Uri.t): result<unit, string> => {
  try {
    await VSCode.Workspace.fs->VSCode.FileSystem.createDirectory(uri)
    Ok()
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}

let delete = async (uri: VSCode.Uri.t): result<unit, string> => {
  try {
    await VSCode.Workspace.fs->VSCode.FileSystem.delete(uri)
    Ok()
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}

let isWritableFileSystem = (uri: VSCode.Uri.t): result<bool, string> => {
  try {
    let isWritable =
      VSCode.Workspace.fs->VSCode.FileSystem.isWritableFileSystem(uri->VSCode.Uri.scheme)
    Ok(isWritable)
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}

let readDirectory = async (uri: VSCode.Uri.t): result<
  array<(string, VSCode.FileType.t)>,
  string,
> => {
  try {
    let entries = await VSCode.Workspace.fs->VSCode.FileSystem.readDirectory(uri)
    Ok(entries)
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}

let readFile = async (uri: VSCode.Uri.t): result<Uint8Array.t, string> => {
  try {
    let content = await VSCode.Workspace.fs->VSCode.FileSystem.readFile(uri)
    Ok(content)
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}

let rename = async (source: VSCode.Uri.t, target: VSCode.Uri.t): result<unit, string> => {
  try {
    await VSCode.Workspace.fs->VSCode.FileSystem.rename(source, target)
    Ok()
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}

let stat = async (uri: VSCode.Uri.t): result<VSCode.FileStat.t, string> => {
  try {
    let fileStat = await VSCode.Workspace.fs->VSCode.FileSystem.stat(uri)
    Ok(fileStat)
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}
