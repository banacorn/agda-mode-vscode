// module for determining the which OS the user is using

// Platforms with prebuilt executables for download
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

// facts about the host that decide which prebuilt executable it can run
type host = {
  os: string, // as in `NodeJs.Os.platform()`
  arch: string, // as in `NodeJs.Os.arch()`
  osRelease: option<osRelease>, // from /etc/os-release, None outside Linux or when it cannot be read
}

// gather the host facts, reading /etc/os-release with `readFile` only on Linux
let readHost = (~os: string, ~arch: string, ~readFile: string => option<string>): host => {
  os,
  arch,
  osRelease: os == "linux" ? readFile("/etc/os-release")->Option.map(parseOsRelease) : None,
}

// whether a VERSION_ID like "24.04" is 24.04 or later, false when it is not a version
let isUbuntu2404OrLater = (versionId: string) =>
  switch String.match(versionId, %re("/^(\d+)\.(\d+)$/")) {
  | Some([_, Some(major), Some(minor)]) =>
    switch (Int.fromString(major), Int.fromString(minor)) {
    | (Some(major), Some(minor)) => major > 24 || (major == 24 && minor >= 4)
    | _ => false
    }
  | _ => false
  }

// decide the platform from the host facts, Error if no prebuilt executable is available for it
let fromHost = (host: host): result<t, host> =>
  switch host.os {
  | "darwin" =>
    switch host.arch {
    | "x64" => Ok(MacOS_Intel)
    | "arm64" => Ok(MacOS_Arm)
    | _ => Error(host)
    }
  | "linux" =>
    // the ALS Ubuntu binary is built for x86_64 on Ubuntu 24.04
    switch (host.osRelease, host.arch) {
    | (Some({id: Some("ubuntu"), versionId: Some(versionId)}), "x64")
      if isUbuntu2404OrLater(versionId) =>
      Ok(Ubuntu)
    | _ => Error(host)
    }
  | "win32" => Ok(Windows)
  | _ => Error(host)
  }

// read a text file, None when it cannot be read
let readTextFile = (path: string): option<string> =>
  try {
    Some(NodeJs.Fs.readFileSync(path)->NodeJs.Buffer.toString)
  } catch {
  | _ => None
  }

// determine the OS the user is using, returns Error if the OS is not supported for downloading prebuilt executables
let determine = async (): result<t, host> =>
  fromHost(readHost(~os=NodeJs.Os.platform(), ~arch=NodeJs.Os.arch(), ~readFile=readTextFile))

let toAssetName = platform =>
  switch platform {
  | Windows => "windows"
  | Ubuntu => "ubuntu"
  | MacOS_Arm => "macos-arm64"
  | MacOS_Intel => "macos-x64"
  | Web => "web"
  }
