// module for determining the which OS the user is using
// see https://github.com/retrohacker/getos for more

// Platforms with prebuilt executables for download
type raw = {"os": string, "dist": string, "codename": string, "release": string}
type t = Windows | Ubuntu | MacOS_Arm | MacOS_Intel | Web

// the fields of /etc/os-release we use, see https://www.freedesktop.org/software/systemd/man/latest/os-release.html
type osRelease = {
  id: option<string>, // ID, like "ubuntu" or "nixos"
  versionId: option<string>, // VERSION_ID, like "24.04"
}

// a value may be in double quotes (with backslash escapes) or single quotes, as in shell
let unquoteOsReleaseValue = (value: string): string => {
  let quotedBy = quote =>
    String.length(value) >= 2 && value->String.startsWith(quote) && value->String.endsWith(quote)
  if quotedBy("\"") {
    value->String.slice(~start=1, ~end=-1)->String.replaceRegExp(%re("/\\(.)/g"), "$1")
  } else if quotedBy("'") {
    value->String.slice(~start=1, ~end=-1)
  } else {
    value
  }
}

// parse the contents of /etc/os-release, ignoring blank lines and comments
let parseOsRelease = (text: string): osRelease => {
  let fields =
    text
    ->String.split("\n")
    ->Array.filterMap(line => {
      let line = String.trim(line)
      switch String.indexOf(line, "=") {
      | -1 => None
      | _ if line->String.startsWith("#") => None
      | i =>
        let key = line->String.slice(~start=0, ~end=i)->String.trim
        let value = line->String.sliceToEnd(~start=i + 1)->String.trim->unquoteOsReleaseValue
        Some((key, value))
      }
    })
    ->Dict.fromArray
  {id: fields->Dict.get("ID"), versionId: fields->Dict.get("VERSION_ID")}
}

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
