type t =
  | LatestALS
  | DevALS

let toDisplayString = channel =>
  switch channel {
  | LatestALS => "Latest Agda Language Server"
  | DevALS => "Development Agda Language Server"
  }

let toString = (channel: t): string =>
  switch channel {
  | LatestALS => "latest"
  | DevALS => "dev"
  }

let fromString = (s: string): option<t> =>
  switch s {
  | "latest" => Some(LatestALS)
  | "dev" => Some(DevALS)
  | _ => None
  }

// DevALS first — it is the default fallback channel when none is persisted
let all: array<t> = [DevALS, LatestALS]

let matchesArtifact = (~channel: t, artifact: Connection__Download__DownloadArtifact.t): bool =>
  switch channel {
  | DevALS => artifact.releaseTag == "dev"
  | LatestALS => artifact.releaseTag != "dev"
  }

let artifactVersionString = (~channel: t, artifact: Connection__Download__DownloadArtifact.t): string =>
  switch channel {
  | DevALS => "Agda v" ++ artifact.agdaVersion ++ " Language Server (dev build)"
  | LatestALS =>
    "Agda v" ++ artifact.agdaVersion ++ " Language Server " ++ Connection__Download__DownloadArtifact.versionLabel(artifact.releaseTag)
  }

module Dev = {
  let makeRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
    username: "agda",
    repository: "agda-language-server",
    userAgent: "banacorn/agda-mode-vscode",
    globalStorageUri,
    cacheInvalidateExpirationSecs: 3600,
  }

  let toDownloadOrder = (releases: array<Connection__Download__GitHub.Release.t>, platform) => {
    let devRelease = releases->Array.find(release => release.tag_name == "dev")
    switch devRelease {
    | None => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
    | Some(devRelease) =>
      let assets = Connection__Download__Assets.forPlatform(devRelease, platform)
      let result =
        assets
        ->Array.toSorted((a, b) =>
          Util.Version.compare(
            Connection__Download__Assets.getAgdaVersionFromAssetName(b),
            Connection__Download__Assets.getAgdaVersionFromAssetName(a),
          )
        )
        ->Array.map(asset => {
          Connection__Download__GitHub.DownloadDescriptor.release: devRelease,
          asset,
          saveAsFileName: "dev-als",
        })
        ->Array.get(0)
      switch result {
      | None => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
      | Some(downloadDescriptor) => Ok(downloadDescriptor)
      }
    }
  }
}

module Latest = {
  let makeRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
    username: "agda",
    repository: "agda-language-server",
    userAgent: "agda/agda-mode-vscode",
    globalStorageUri,
    cacheInvalidateExpirationSecs: 86400,
  }

  let toDownloadOrder = (releases: array<Connection__Download__GitHub.Release.t>, platform) => {
    let stableReleases =
      releases->Array.filter(release =>
        release.tag_name != "dev" &&
          !release.prerelease &&
          !release.draft &&
          Date.fromString(release.published_at) >= Date.fromString("2024-12-18")
      )

    let compareTagNames = (a, b) => {
      let stripV = s => s->String.startsWith("v") ? String.sliceToEnd(s, ~start=1) : s
      let partsA = stripV(a)->String.split(".")
      let partsB = stripV(b)->String.split(".")
      let maxLen = max(Array.length(partsA), Array.length(partsB))
      let result = ref(0.)
      let i = ref(0)
      while result.contents == 0. && i.contents < maxLen {
        let numA =
          partsA->Array.get(i.contents)->Option.flatMap(s => Int.fromString(s))->Option.getOr(0)
        let numB =
          partsB->Array.get(i.contents)->Option.flatMap(s => Int.fromString(s))->Option.getOr(0)
        result := if numA < numB {
          -1.
        } else if numA > numB {
          1.
        } else {
          0.
        }
        i := i.contents + 1
      }
      result.contents
    }

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

    let found = sortedReleases->Array.reduce(None, (acc, release) =>
      switch acc {
      | Some(_) => acc
      | None =>
        let assets =
          Connection__Download__Assets.forPlatform(release, platform)
          ->Array.toSorted((a, b) =>
            Util.Version.compare(
              Connection__Download__Assets.getAgdaVersionFromAssetName(b),
              Connection__Download__Assets.getAgdaVersionFromAssetName(a),
            )
          )
        switch assets->Array.get(0) {
        | None => None
        | Some(asset) =>
          Some({
            Connection__Download__GitHub.DownloadDescriptor.release,
            asset,
            saveAsFileName: "latest-als",
          })
        }
      }
    )

    switch found {
    | None => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
    | Some(downloadDescriptor) => Ok(downloadDescriptor)
    }
  }
}
