// Loads a compiled ReScript module that transitively `require("vscode")`
// (which doesn't exist outside VS Code -- it's normally injected by the
// extension host at runtime), by registering a fake module for that one
// require call.
//
// Used for two different compiled modules with two different stub needs:
//  - WebviewPanel.bs.js actually calls `vscode.window.createWebviewPanel`
//    and a handful of `Uri`/`Webview` methods (see harness.js), so it needs
//    a stub that implements those.
//  - Repro338.bs.js only needs `require("vscode")` to resolve to
//    *something*, since the modules it pulls in -- Common.bs.js,
//    Link.bs.js, Item.bs.js -- reference `vscode` only inside function
//    bodies, never at module load (see repro-338.js).
//
// Patching/restoring `Module._load` and `require()` are both synchronous;
// nothing here needs to be async.

const path = require("node:path")
const Module = require("node:module")

const repoRoot = path.resolve(__dirname, "..", "..")

// Installs `stub` for the duration of `fn`, then restores the original
// module loader -- even if `fn` throws.
const withVscodeStub = (stub, fn) => {
  const originalLoad = Module._load
  Module._load = function (request, parent, isMain) {
    if (request === "vscode") return stub
    return originalLoad.apply(this, arguments)
  }
  try {
    return fn()
  } finally {
    Module._load = originalLoad
  }
}

// Requires a compiled module (path relative to the repo root) with `stub`
// installed for `require("vscode")`. The module keeps whatever reference it
// captured at load time, so it works fine even after the stub is restored.
const requireCompiled = (relativePath, stub = {}) =>
  withVscodeStub(stub, () => require(path.join(repoRoot, relativePath)))

module.exports = {withVscodeStub, requireCompiled, repoRoot}
