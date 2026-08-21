// Serves the panel webview exactly as VS Code would, for testing outside VS Code.
//
// Rather than hand-maintaining a copy of the HTML VS Code generates for the
// webview, this calls the real `WebviewPanel.make` (src/View/WebviewPanel.res,
// compiled to lib/js/src/View/WebviewPanel.bs.js) with a minimal stub of the
// `vscode` module, and serves the exact HTML string it produces (plus the
// real dist/bundled-view.js and dist/style.css it references) over a local
// HTTP server. `WebviewPanel.make` only touches `vscode.window.createWebviewPanel`
// and a handful of `Uri`/`Webview` methods/properties -- none of it is called
// at module-load time, so this stub only needs to cover what's actually
// invoked, not the whole `vscode` API surface.
//
// This can't drift from what VS Code actually serves, because it isn't a
// copy -- it's the same code path.

const http = require("node:http")
const path = require("node:path")
const fs = require("node:fs")
const Module = require("node:module")

const repoRoot = path.resolve(__dirname, "..", "..")

const makeUri = fsPath => ({
  fsPath,
  toString: () => "file://" + fsPath,
})

// Registers the fake `vscode` module for `require("vscode")` calls made
// while loading WebviewPanel.bs.js (and whatever it transitively requires).
// Must run before that require happens.
const installVscodeStub = ({ origin, capture }) => {
  const originalLoad = Module._load
  Module._load = function (request, parent, isMain) {
    if (request === "vscode") {
      return {
        window: {
          createWebviewPanel: (_viewType, _title, _showOptions, _options) => ({
            webview: {
              cspSource: origin,
              asWebviewUri: uri => {
                const url = origin + "/" + path.relative(repoRoot, uri.fsPath)
                return {fsPath: url, toString: () => url}
              },
              set html(value) {
                capture.html = value
              },
            },
          }),
        },
        Uri: {
          file: fsPath => makeUri(fsPath),
          joinPath: (base, ...segments) => makeUri(path.join(base.fsPath, ...segments)),
        },
      }
    }
    return originalLoad.apply(this, arguments)
  }
  return () => {
    Module._load = originalLoad
  }
}

const contentType = filePath => {
  if (filePath.endsWith(".js")) return "application/javascript"
  if (filePath.endsWith(".css")) return "text/css"
  if (filePath.endsWith(".html")) return "text/html"
  return "application/octet-stream"
}

// Starts the server, generates the real webview HTML, and resolves once the
// harness page (with `acquireVsCodeApi` stubbed in) is being served.
// Returns { url, close }.
const start = async () => {
  const capture = {html: null}

  const server = http.createServer((req, res) => {
    if (req.url === "/harness") {
      // Served verbatim: the page's own CSP (`script-src 'nonce-...'`) would
      // reject an inline script spliced in here. `acquireVsCodeApi` --
      // which only exists inside a real VS Code webview host -- must be
      // stubbed by the caller via `page.addInitScript` instead, since that's
      // injected before any page script runs and isn't subject to the
      // page's CSP.
      res.writeHead(200, {"Content-Type": "text/html"})
      res.end(capture.html)
      return
    }

    const filePath = path.join(repoRoot, decodeURIComponent(req.url))
    if (!filePath.startsWith(repoRoot) || !fs.existsSync(filePath)) {
      res.writeHead(404)
      res.end("Not found: " + req.url)
      return
    }
    res.writeHead(200, {"Content-Type": contentType(filePath)})
    fs.createReadStream(filePath).pipe(res)
  })

  await new Promise(resolve => server.listen(0, "127.0.0.1", resolve))
  const {port} = server.address()
  const origin = `http://127.0.0.1:${port}`

  const restoreLoad = installVscodeStub({origin, capture})
  let WebviewPanel
  try {
    // eslint-disable-next-line global-require
    WebviewPanel = require(path.join(repoRoot, "lib/js/src/View/WebviewPanel.bs.js"))
  } finally {
    restoreLoad()
  }

  WebviewPanel.WebviewPanel.make("Agda Mode", makeUri(repoRoot))

  if (!capture.html) {
    server.close()
    throw new Error("WebviewPanel.make did not call webview.setHtml -- no HTML captured")
  }

  return {
    url: `${origin}/harness`,
    close: () => new Promise(resolve => server.close(resolve)),
  }
}

module.exports = {start}
