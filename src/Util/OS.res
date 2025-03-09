// NOTE: cannot be used in the View module
let onUnix = switch NodeJs.Os.platform() {
| "win32" => false
| _ => true
}
