open Mocha

module Candidate = Connection__Candidate

let makeResolved = (raw: string, resource: VSCode.Uri.t): Candidate.Resolved.t => {
  original: Candidate.make(raw),
  resource,
}

describe("Memento", () => {
  describe("PreferredCandidate", () => {
    Async.it("should round-trip set/get", async () => {
      let memento = Memento.make(None)

      await Memento.PreferredCandidate.set(memento, Some("/usr/local/bin/agda"))

      Assert.deepStrictEqual(
        Memento.PreferredCandidate.get(memento),
        Some("/usr/local/bin/agda"),
      )
    })

    Async.it("should clear stored value", async () => {
      let memento = Memento.make(None)

      await Memento.PreferredCandidate.set(memento, Some("/usr/local/bin/agda"))
      await Memento.PreferredCandidate.clear(memento)

      Assert.deepStrictEqual(Memento.PreferredCandidate.get(memento), None)
    })
  })

  describe("SelectedChannel", () => {
    Async.it("should round-trip set/get", async () => {
      let memento = Memento.make(None)

      await Memento.SelectedChannel.set(memento, "DevALS")

      Assert.deepStrictEqual(Memento.SelectedChannel.get(memento), Some("DevALS"))
    })

    Async.it("should clear stored value", async () => {
      let memento = Memento.make(None)

      await Memento.SelectedChannel.set(memento, "DevALS")
      await Memento.SelectedChannel.clear(memento)

      Assert.deepStrictEqual(Memento.SelectedChannel.get(memento), None)
    })
  })

  describe("ResolvedMetadata", () => {
    Async.it("should round-trip setKind/get", async () => {
      let memento = Memento.make(None)
      let resolved = makeResolved("/usr/local/bin/agda", VSCode.Uri.file("/usr/local/bin/agda"))

      await Memento.ResolvedMetadata.setKind(
        memento,
        resolved,
        Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
      )

      Assert.deepStrictEqual(
        Memento.ResolvedMetadata.get(memento, resolved)->Option.map(entry => entry.kind),
        Some(Memento.ResolvedMetadata.Agda(Some("2.7.0.1"))),
      )
    })

    Async.it("should set Unknown kind when setError is called first", async () => {
      let memento = Memento.make(None)
      let resolved = makeResolved("/usr/local/bin/agda", VSCode.Uri.file("/usr/local/bin/agda"))

      await Memento.ResolvedMetadata.setError(memento, resolved, "boom")

      Assert.deepStrictEqual(
        Memento.ResolvedMetadata.get(memento, resolved)->Option.map(entry => (entry.kind, entry.error)),
        Some((Memento.ResolvedMetadata.Unknown, Some("boom"))),
      )
    })

    Async.it("should preserve existing kind when setError is called after setKind", async () => {
      let memento = Memento.make(None)
      let resolved = makeResolved(
        "vscode-userdata:/global/als.wasm",
        VSCode.Uri.parse("vscode-userdata:/global/als.wasm"),
      )

      await Memento.ResolvedMetadata.setKind(
        memento,
        resolved,
        Memento.ResolvedMetadata.ALS(WASM, Some(("1.2.3", "2.7.0.1", None))),
      )
      await Memento.ResolvedMetadata.setError(memento, resolved, "broken")

      Assert.deepStrictEqual(
        Memento.ResolvedMetadata.get(memento, resolved)->Option.map(entry => (entry.kind, entry.error)),
        Some((Memento.ResolvedMetadata.ALS(WASM, Some(("1.2.3", "2.7.0.1", None))), Some("broken"))),
      )
    })

    Async.it("should replace previous error when setKind is called", async () => {
      let memento = Memento.make(None)
      let resolved = makeResolved("/usr/local/bin/agda", VSCode.Uri.file("/usr/local/bin/agda"))

      await Memento.ResolvedMetadata.setError(memento, resolved, "broken")
      await Memento.ResolvedMetadata.setKind(
        memento,
        resolved,
        Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
      )

      Assert.deepStrictEqual(
        Memento.ResolvedMetadata.get(memento, resolved)->Option.map(entry => (entry.kind, entry.error)),
        Some((Memento.ResolvedMetadata.Agda(Some("2.7.0.1")), None)),
      )
    })

    Async.it("should clear all entries", async () => {
      let memento = Memento.make(None)
      let resolvedA = makeResolved("/usr/local/bin/agda", VSCode.Uri.file("/usr/local/bin/agda"))
      let resolvedB = makeResolved(
        "vscode-userdata:/global/als.wasm",
        VSCode.Uri.parse("vscode-userdata:/global/als.wasm"),
      )

      await Memento.ResolvedMetadata.setKind(
        memento,
        resolvedA,
        Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
      )
      await Memento.ResolvedMetadata.setError(memento, resolvedB, "broken")
      await Memento.ResolvedMetadata.clear(memento)

      Assert.deepStrictEqual(Memento.ResolvedMetadata.entries(memento)->Dict.toArray->Array.length, 0)
      Assert.deepStrictEqual(Memento.ResolvedMetadata.get(memento, resolvedA), None)
      Assert.deepStrictEqual(Memento.ResolvedMetadata.get(memento, resolvedB), None)
    })

    Async.it("should clear only entries under the given directories", async () => {
      let memento = Memento.make(None)
      let managedDir = VSCode.Uri.file("/tmp/hardcoded-als")
      let managedResolved = makeResolved(
        "/tmp/hardcoded-als/als",
        VSCode.Uri.file("/tmp/hardcoded-als/als"),
      )
      let keptResolved = makeResolved(
        "/usr/local/bin/agda",
        VSCode.Uri.file("/usr/local/bin/agda"),
      )

      await Memento.ResolvedMetadata.setKind(
        memento,
        managedResolved,
        Memento.ResolvedMetadata.ALS(Native, None),
      )
      await Memento.ResolvedMetadata.setKind(
        memento,
        keptResolved,
        Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
      )

      await Memento.ResolvedMetadata.clearUnderDirectories(memento, [managedDir])

      Assert.deepStrictEqual(Memento.ResolvedMetadata.get(memento, managedResolved), None)
      Assert.deepStrictEqual(
        Memento.ResolvedMetadata.get(memento, keptResolved)->Option.map(entry => entry.kind),
        Some(Memento.ResolvedMetadata.Agda(Some("2.7.0.1"))),
      )
    })
  })

  describe("ALSReleaseCache", () => {
    Async.it("should round-trip timestamp and releases", async () => {
      let memento = Memento.make(None)
      let now = Date.make()

      await Memento.ALSReleaseCache.setTimestamp(
        memento,
        "agda",
        "agda-language-server",
        now,
      )
      await Memento.ALSReleaseCache.setReleases(
        memento,
        "agda",
        "agda-language-server",
        "cached-release-json",
      )

      let timestamp =
        Memento.ALSReleaseCache.getTimestamp(memento, "agda", "agda-language-server")
        ->Option.map(Date.toString)
      let releases: option<string> =
        Memento.ALSReleaseCache.getReleases(memento, "agda", "agda-language-server")

      Assert.deepStrictEqual(timestamp, Some(Date.toString(now)))
      Assert.deepStrictEqual(releases, Some("cached-release-json"))
    })

    Async.it("should report cache age after timestamp is stored", async () => {
      let memento = Memento.make(None)

      await Memento.ALSReleaseCache.setTimestamp(
        memento,
        "agda",
        "agda-language-server",
        Date.make(),
      )

      Assert.deepStrictEqual(
        Memento.ALSReleaseCache.getCacheAgeInSecs(memento, "agda", "agda-language-server")
        ->Option.isSome,
        true,
      )
    })

    Async.it("should clear only the targeted repository", async () => {
      let memento = Memento.make(None)
      let now = Date.make()

      await Memento.ALSReleaseCache.setTimestamp(
        memento,
        "agda",
        "agda-language-server",
        now,
      )
      await Memento.ALSReleaseCache.setReleases(
        memento,
        "agda",
        "agda-language-server",
        "agda-cache",
      )
      await Memento.ALSReleaseCache.setTimestamp(memento, "other", "repo", now)
      await Memento.ALSReleaseCache.setReleases(memento, "other", "repo", "other-cache")

      await Memento.ALSReleaseCache.clear(memento, "agda", "agda-language-server")

      let agdaReleases: option<string> =
        Memento.ALSReleaseCache.getReleases(memento, "agda", "agda-language-server")
      let otherReleases: option<string> =
        Memento.ALSReleaseCache.getReleases(memento, "other", "repo")

      Assert.deepStrictEqual(
        Memento.ALSReleaseCache.getTimestamp(memento, "agda", "agda-language-server"),
        None,
      )
      Assert.deepStrictEqual(agdaReleases, None)
      Assert.deepStrictEqual(
        Memento.ALSReleaseCache.getTimestamp(memento, "other", "repo")->Option.isSome,
        true,
      )
      Assert.deepStrictEqual(otherReleases, Some("other-cache"))
    })
  })
})
