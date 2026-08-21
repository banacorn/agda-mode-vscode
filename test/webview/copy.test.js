// Regression test for #338: copying a "Rich" mode expression (e.g. a goal's
// type) from the panel pastes with every token on its own line.
//
// Loads the real webview bundle (dist/bundled-view.js + dist/style.css) via
// harness.js, feeds it the real `Display` event JSON for the issue's
// expression (test/webview/Issue338.res), selects the rendered text the
// same way a user would, and asserts the browser's own text serialization
// -- the same mechanism a real copy uses -- doesn't contain stray newlines.
//
// `Selection.toString()` (not `element.innerText()`, which is just a proxy
// for the same thing) is used deliberately: it requires a real layout
// engine, which is exactly why this can't be run under jsdom -- jsdom has
// no layout engine and so cannot reproduce Chromium's per-flex-item
// copy-newline quirk even in principle. See #338 for the full writeup.

const assert = require("node:assert")
const {chromium} = require("playwright")
const harness = require("./harness.js")
const {requireCompiled} = require("./vscode-stub.js")

const run = async () => {
  const {json, expectedCopyText} = requireCompiled("lib/js/test/webview/Issue338.bs.js")
  const {url, close: closeHarness} = await harness.start()
  const browser = await chromium.launch()

  try {
    const page = await browser.newPage()
    // `acquireVsCodeApi` only exists inside a real VS Code webview host;
    // injected before any page script runs, so it isn't subject to the
    // page's own CSP (unlike an inline <script> spliced into the HTML).
    await page.addInitScript(() => {
      window.acquireVsCodeApi = () => ({
        postMessage: () => {},
        setState: () => {},
        getState: () => undefined,
      })
    })

    const pageErrors = []
    page.on("pageerror", err => pageErrors.push(err))

    await page.goto(url)
    await page.evaluate(message => window.postMessage(message, "*"), json)
    await page.waitForSelector(".component-horz")

    assert.deepStrictEqual(pageErrors, [], "webview threw while rendering the repro payload")

    const selectedText = await page.evaluate(() => {
      const el = document.querySelector(".component-horz")
      const range = document.createRange()
      range.selectNodeContents(el)
      const selection = window.getSelection()
      selection.removeAllRanges()
      selection.addRange(range)
      return selection.toString()
    })

    assert.strictEqual(
      selectedText,
      expectedCopyText,
      "selecting and copying a Rich-mode expression should yield space-separated " +
        "text on one line, not a newline per token (#338)",
    )
  } finally {
    await browser.close()
    await closeHarness()
  }
}

run()
  .then(() => {
    console.log("ok - copy.test.js")
  })
  .catch(err => {
    console.error("not ok - copy.test.js")
    console.error(err)
    process.exitCode = 1
  })
