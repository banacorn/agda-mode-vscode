// Web-compatible module for unzipping downloaded files using JSZip
module JSZip = {
  type t
  type zipObject = {
    "name": string,
    "dir": bool,
    "async": (. string) => promise<Uint8Array.t>,
  }
  
  @new @module("jszip") external make: unit => t = "default"
  @send external loadAsync: (t, Uint8Array.t) => promise<t> = "loadAsync"
  @send external forEach: (t, (. string, zipObject) => unit) => unit = "forEach"
}

// Web-compatible unzip implementation using JSZip and VS Code file system
let run = async (srcUri: VSCode.Uri.t, destUri: VSCode.Uri.t): result<unit, string> => {
  try {
    // Read the ZIP file as Uint8Array
    let zipData = switch await FS.readFile(srcUri) {
    | Error(error) => raise(Failure("Cannot read ZIP file: " ++ error))
    | Ok(data) => data
    }
    
    // Load ZIP with JSZip
    let jszip = JSZip.make()
    let loadedZip = await jszip->JSZip.loadAsync(zipData)
    
    // Extract all files
    let extractFile = async (fileName: string, zipObject: JSZip.zipObject) => {
      if !zipObject["dir"] {
        // It's a file, extract it
        let fileData = await zipObject["async"](. "uint8array")
        let fileUri = VSCode.Uri.joinPath(destUri, [fileName])
        
        // Create parent directories if needed (VS Code handles this automatically)
        switch await FS.writeFile(fileUri, fileData) {
        | Error(error) => raise(Failure("Cannot write extracted file " ++ fileName ++ ": " ++ error))
        | Ok() => ()
        }
      }
    }
    
    // Create promise array for all file extractions
    let extractPromises = ref([])
    
    loadedZip->JSZip.forEach((. fileName, zipObject) => {
      let promise = extractFile(fileName, zipObject)
      extractPromises := Array.push(extractPromises.contents, promise)
    })
    
    // Wait for all extractions to complete
    await Promise.all(extractPromises.contents)
    
    Ok()
  } catch {
  | Js.Exn.Error(exn) => 
    let message = Js.Exn.message(exn)->Option.getOr("Unknown error during ZIP extraction")
    Error(message)
  | Failure(msg) => Error(msg)
  | _ => Error("Unknown error during ZIP extraction")
  }
}
