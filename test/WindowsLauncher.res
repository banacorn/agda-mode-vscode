// Agda is spawned without a shell (#387), and Windows refuses to start a `.bat`
// without one. This compiles, once per test run, a small `.exe` that runs the
// `.bat` of the same name next to it, so batch-file mocks can be started directly.
@module("node:child_process")
external execFile: (string, array<string>, (Nullable.t<Js.Exn.t>, string, string) => unit) => unit =
  "execFile"
@module("node:fs") external copyFileSync: (string, string) => unit = "copyFileSync"

let source = `using System.Diagnostics;
using System.IO;
using System.Reflection;
class Launcher {
  static int Main(string[] args) {
    string q = ((char)34).ToString();
    string script = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, ".bat");
    ProcessStartInfo info = new ProcessStartInfo("cmd.exe", "/d /s /c " + q + q + script + q + " " + string.Join(" ", args) + q);
    info.UseShellExecute = false;
    Process child = Process.Start(info);
    child.WaitForExit();
    return child.ExitCode;
  }
}
`

// The compile is kept as a promise, so a caller that arrives while `csc.exe` is
// still running waits for that compile instead of starting another one.
let compiled: ref<option<promise<string>>> = ref(None)

// `csc.exe` runs asynchronously, so the extension host keeps responding while it compiles
let compile = () =>
  switch compiled.contents {
  | Some(promise) => promise
  | None =>
    let windir =
      NodeJs.Process.process->NodeJs.Process.env->Dict.get("WINDIR")->Option.getOr("C:\\Windows")
    let csc = NodeJs.Path.join([windir, "Microsoft.NET", "Framework64", "v4.0.30319", "csc.exe"])
    let sourcePath = NodeJs.Path.join([NodeJs.Os.tmpdir(), "agda-mode-mock-launcher.cs"])
    let exePath = NodeJs.Path.join([NodeJs.Os.tmpdir(), "agda-mode-mock-launcher.exe"])
    NodeJs.Fs.writeFileSync(sourcePath, NodeJs.Buffer.fromString(source))
    let promise = Promise.make((resolve, reject) =>
      execFile(csc, ["/nologo", "/out:" ++ exePath, sourcePath], (error, _, _) =>
        switch Nullable.toOption(error) {
        | Some(error) => reject(error)
        | None => resolve(exePath)
        }
      )
    )
    compiled := Some(promise)
    promise
  }

// given the path of a `.bat`, puts a launcher `.exe` next to it and returns the `.exe`'s path
let wrap = async batPath => {
  let exePath = String.slice(batPath, ~start=0, ~end=String.length(batPath) - 4) ++ ".exe"
  copyFileSync(await compile(), exePath)
  exePath
}
