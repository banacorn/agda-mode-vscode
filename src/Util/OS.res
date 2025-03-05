// NOTE: cannot be used in the View module
let onUnix = switch NodeJs.Os.type_() {
| "Windows_NT" => false
| _ => true
}
