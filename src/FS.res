// Exception-safe interface to VSCode.FileSystem
// Returns result types instead of throwing exceptions

let readDirectory = async (uri: VSCode.Uri.t): result<array<(string, VSCode.FileType.t)>, string> => {
  try {
    let entries = await VSCode.Workspace.fs->VSCode.FileSystem.readDirectory(uri)
    Ok(entries)
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}

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
    let isWritable = VSCode.Workspace.fs->VSCode.FileSystem.isWritableFileSystem(uri)
    Ok(isWritable)
  } catch {
  | Js.Exn.Error(obj) => Error(Js.Exn.message(obj)->Option.getOr("Unknown file system error"))
  | _ => Error("Unknown file system error")
  }
}