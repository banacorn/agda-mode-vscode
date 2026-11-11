open Mocha

// Resolve the real VS Code configuration with a language scope: inspecting
// package.json alone would miss override precedence and effective defaults.
type languageScope = {languageId: string}

@module("vscode") @scope("workspace")
external configurationForLanguage: (string, languageScope) => VSCode.WorkspaceConfiguration.t =
  "getConfiguration"

describe("semantic highlighting defaults (#139)", () => {
  it("enables semantic colors for every Agda dialect independently of the theme", () => {
    LanguageId.all->Array.forEach(
      languageId => {
        let actual =
          configurationForLanguage(
            "editor",
            {languageId: languageId},
          )->VSCode.WorkspaceConfiguration.get("semanticHighlighting.enabled")
        Assert.deepStrictEqual(actual, Some(true), ~message=languageId)
      },
    )
  })
})
