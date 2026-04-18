let makeRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
  username: "agda",
  repository: "agda-language-server",
  userAgent: "agda/agda-mode-vscode",
  globalStorageUri,
  cacheInvalidateExpirationSecs: 86400,
}

let chooseAssetByPlatform = (release: Connection__Download__GitHub.Release.t, platform): array<
  Connection__Download__GitHub.Asset.t,
> => Connection__Download__Assets.nativeForPlatform(release, platform)

let getAgdaVersionFromAssetName = (asset: Connection__Download__GitHub.Asset.t) =>
  Connection__Download__Assets.getAgdaVersionFromAssetName(asset)

// Given a list of releases, choose the latest compatible release for the given platform
let toDownloadOrder = (releases: array<Connection__Download__GitHub.Release.t>, platform) => {
  // only stable (non-dev, non-prerelease, non-draft) releases after 2024-12-18 are considered
  let stableReleases =
    releases->Array.filter(release =>
      release.tag_name != "dev" &&
        !release.prerelease &&
        !release.draft &&
        Date.fromString(release.published_at) >= Date.fromString("2024-12-18")
    )

  // Compare tag names part-by-part numerically (handles non-semver like v0.2.7.0.1.5)
  let compareTagNames = (a, b) => {
    let stripV = s => s->String.startsWith("v") ? String.sliceToEnd(s, ~start=1) : s
    let partsA = stripV(a)->String.split(".")
    let partsB = stripV(b)->String.split(".")
    let maxLen = max(Array.length(partsA), Array.length(partsB))
    let result = ref(0.)
    let i = ref(0)
    while result.contents == 0. && i.contents < maxLen {
      let numA = partsA->Array.get(i.contents)->Option.flatMap(s => Int.fromString(s))->Option.getOr(0)
      let numB = partsB->Array.get(i.contents)->Option.flatMap(s => Int.fromString(s))->Option.getOr(0)
      result := if numA < numB { -1. } else if numA > numB { 1. } else { 0. }
      i := i.contents + 1
    }
    result.contents
  }

  // canonical latest: sort by published_at descending, then version descending as tiebreaker
  let sortedReleases = stableReleases->Array.toSorted((a, b) => {
    let timeA = Date.getTime(Date.fromString(a.published_at))
    let timeB = Date.getTime(Date.fromString(b.published_at))
    if timeB > timeA {
      1.
    } else if timeB < timeA {
      -1.
    } else {
      compareTagNames(b.tag_name, a.tag_name)
    }
  })

  // pick the first release in sorted order that has a compatible asset for the platform
  let found = sortedReleases->Array.reduce(None, (acc, release) =>
    switch acc {
    | Some(_) => acc
    | None =>
      let assets =
        chooseAssetByPlatform(release, platform)
        ->Array.toSorted((a, b) =>
          Util.Version.compare(getAgdaVersionFromAssetName(b), getAgdaVersionFromAssetName(a))
        )
      switch assets->Array.get(0) {
      | None => None
      | Some(asset) =>
        Some(
          Connection__Download.Source.FromGitHub(
            LatestALS,
            {
              Connection__Download__GitHub.DownloadDescriptor.release,
              asset,
              saveAsFileName: "latest-als",
            },
          ),
        )
      }
    }
  )

  switch found {
  | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
  | Some(source) => Ok(source)
  }
}
