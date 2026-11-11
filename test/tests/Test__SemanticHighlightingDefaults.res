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

  it("leaves other languages' semantic highlighting defaults alone", () => {
    ["javascript", "markdown", "typst", "org", "forester"]->Array.forEach(
      languageId => {
        let actual =
          configurationForLanguage(
            "editor",
            {languageId: languageId},
          )->VSCode.WorkspaceConfiguration.get("semanticHighlighting.enabled")
        Assert.deepStrictEqual(actual, Some("configuredByTheme"), ~message=languageId)
      },
    )
  })

  Async.it(
    "honors an explicit Agda language override and restores the default when removed",
    async () => {
      let config = configurationForLanguage("editor", {languageId: "agda"})
      // The runner uses a fresh temporary profile; always remove this override,
      // including when the assertion fails, so it cannot affect other tests.
      let failure = try {
        await config->VSCode.WorkspaceConfiguration.updateGlobalSettings(
          "semanticHighlighting.enabled",
          false,
          Some(true),
        )
        Assert.deepStrictEqual(
          configurationForLanguage(
            "editor",
            {languageId: "agda"},
          )->VSCode.WorkspaceConfiguration.get("semanticHighlighting.enabled"),
          Some(false),
        )
        None
      } catch {
      | exn => Some(exn)
      }
      await config->VSCode.WorkspaceConfiguration.updateGlobalSettings(
        "semanticHighlighting.enabled",
        None,
        Some(true),
      )
      switch failure {
      | Some(exn) => raise(exn)
      | None => ()
      }
      Assert.deepStrictEqual(
        configurationForLanguage("editor", {languageId: "agda"})->VSCode.WorkspaceConfiguration.get(
          "semanticHighlighting.enabled",
        ),
        Some(true),
      )
    },
  )
})
