// module for determining the which OS the user is using
// see https://github.com/retrohacker/getos for more

// Platforms with prebuilt executables for download
type t = Windows | Ubuntu | MacOS_Arm | MacOS_Intel

// binding to the getos module
@module
external getos: (
  ('e, {"os": string, "dist": string, "codename": string, "release": string}) => unit
) => unit = "getos"

// determine the OS the user is using, returns None if the OS is not supported for downloading prebuilt executables
let determine = (): promise<option<t>> =>
  Promise.make((resolve, _) => {
    switch NodeJs.Os.platform() {
    | "darwin" =>
      switch NodeJs.Os.arch() {
      | "x64" => resolve(Some(MacOS_Intel))
      | "arm64" => resolve(Some(MacOS_Arm))
      | _ => resolve(None)
      }
    | "linux" =>
      getos((error, os) => {
        switch Js.Nullable.toOption(error) {
        | Some(_) => resolve(None)
        | None =>
          switch os["dist"] {
          | "Ubuntu" => resolve(Some(Ubuntu))
          | _ => resolve(None)
          }
        }
      })
    | "win32" => resolve(Some(Windows))
    | _ => resolve(None)
    }
  })

let toAssetName = platform =>
  switch platform {
  | Windows => "windows"
  | Ubuntu => "ubuntu"
  | MacOS_Arm => "macos-arm64"
  | MacOS_Intel => "macos-x64"
  }
