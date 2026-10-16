// Regression test for #34: source locations in the panel are not styled as
// links, because their class names are glued together instead of joined with
// spaces, so `.component-link` and `.component-link:hover` never match.
//
// Loads the real webview bundle (dist/bundled-view.js + dist/style.css) via
// harness.js, feeds it the real `Display` event JSON for an error message
// with a source location (test/webview/Issue34.res), and checks the rendered
// class lists and the computed style of the hovered link.

const assert = require("node:assert")
const {chromium} = require("playwright")
const harness = require("./harness.js")
const {requireCompiled} = require("./vscode-stub.js")

const run = async () => {
  const {json} = requireCompiled("lib/js/test/webview/Issue34.bs.js")
  const {url, close: closeHarness} = await harness.start()
  const browser = await chromium.launch()

  try {
    const page = await browser.newPage()
    // `acquireVsCodeApi` only exists inside a real VS Code webview host.
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
    // Theme colors VS Code would provide, so the link colors are observable.
    await page.evaluate(() => {
      const style = document.documentElement.style
      style.setProperty("--vscode-textLink-foreground", "rgb(1, 2, 3)")
      style.setProperty("--vscode-textLink-activeForeground", "rgb(4, 5, 6)")
    })
    await page.evaluate(message => window.postMessage(message, "*"), json)
    await page.waitForSelector(".item-content")

    assert.deepStrictEqual(pageErrors, [], "webview threw while rendering the repro payload")

    // Matched by substring, so they're found whether or not the class names
    // are joined correctly.
    const classLists = selector =>
      page.evaluate(
        selector =>
          Array.from(document.querySelectorAll(selector), el => Array.from(el.classList)),
        selector,
      )

    assert.deepStrictEqual(
      await classLists('.item-content [class*="component-link"]'),
      [["component-link", "component-hole"]],
      "a source location should be one element with separate component-link and component-hole classes",
    )
    assert.deepStrictEqual(
      await classLists('.item-content [class*="codicon"]'),
      [["codicon", "codicon-link"]],
      "a source location's icon should have separate codicon and codicon-link classes",
    )

    const link = page.locator('.item-content [class*="component-link"]')
    const linkStyle = () =>
      link.evaluate(el => {
        const style = getComputedStyle(el)
        return {
          color: style.color,
          cursor: style.cursor,
          textDecorationLine: style.textDecorationLine,
        }
      })

    assert.deepStrictEqual(
      await linkStyle(),
      {color: "rgb(1, 2, 3)", cursor: "auto", textDecorationLine: "none"},
      "a source location should have the link color",
    )

    await link.hover()

    assert.deepStrictEqual(
      await linkStyle(),
      {color: "rgb(4, 5, 6)", cursor: "pointer", textDecorationLine: "underline"},
      "a hovered source location should have the link hover style",
    )
  } finally {
    await browser.close()
    await closeHarness()
  }
}

run()
  .then(() => {
    console.log("ok - link.test.js")
  })
  .catch(err => {
    console.error("not ok - link.test.js")
    console.error(err)
    process.exitCode = 1
  })
