// module for determining the which OS the user is using
// see https://github.com/retrohacker/getos for more

// Platforms with prebuilt executables for download
type raw = {"os": string, "dist": string, "codename": string, "release": string}
type t = Windows | Ubuntu | MacOS_Arm | MacOS_Intel | Web

// binding to the getos module
@module
external getos: (('e, raw) => unit) => unit = "getos"

// determine the OS the user is using, returns None if the OS is not supported for downloading prebuilt executables
let determine = (): promise<result<t, raw>> =>
  Promise.make((resolve, _) => {
    getos((error, raw) => {
      switch Js.Nullable.toOption(error) {
      | Some(_) => resolve(Error(raw))
      | None =>
        switch NodeJs.Os.platform() {
        | "darwin" =>
          switch NodeJs.Os.arch() {
          | "x64" => resolve(Ok(MacOS_Intel))
          | "arm64" => resolve(Ok(MacOS_Arm))
          | _ => resolve(Error(raw))
          }
        | "linux" =>
          switch raw["dist"] {
          | "Ubuntu" => resolve(Ok(Ubuntu))
          | _ => resolve(Error(raw))
          }
        | "win32" => resolve(Ok(Windows))
        | _ => resolve(Error(raw))
        }
      }
    })
  })

let toAssetName = platform =>
  switch platform {
  | Windows => "windows"
  | Ubuntu => "ubuntu"
  | MacOS_Arm => "macos-arm64"
  | MacOS_Intel => "macos-x64"
  | Web => "web"
  }
