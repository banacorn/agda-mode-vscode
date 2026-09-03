# Introduction

This is a Visual Studio Code extension for Agda. 

Having some frontend development skills will definitely help you contribute to this project (but don't worry, you'll manage even if you don't have them)

### Language

This project is written in [ReScript](https://rescript-lang.org/). It's essentially OCaml that compiles to JavaScript, so that we don't have to write JavaScript ourselves. 

### Framework

We use [React](https://rescript-lang.org/docs/react/latest/introduction) as the frontend framework. It comes with a nice binding for ReScript.

# Setup

You'll need to install [Node.js](https://nodejs.org/) for building the project. 

After cloning the files, download dependencies and build files with:

```bash
npm install 
npm run build
```

Fire up this command to enter the "watch mode" so that you don't have to rebuild stuff manually:

```bash 
npm run dev
```

Press <kbd>F5</kbd> in VS Code and you should have a extension development host with agda-mode running!

# Versioning Policy

This extension follows VS Code's recommended versioning scheme for extensions:

- **Release versions**: Use even minor numbers (e.g., `0.6.x`, `0.8.x`, `1.0.x`)
- **Prerelease versions**: Use odd minor numbers (e.g., `0.7.x`, `0.9.x`, `1.1.x`)

## Examples:
- `0.6.1` - Stable release
- `0.7.0` - First prerelease with new features (e.g., web support)
- `0.7.1` - Prerelease bug fixes and improvements
- `0.8.0` - Next stable release incorporating prerelease features

## Publishing:
- **Release versions** are published as stable extensions
- **Prerelease versions** are published with `preRelease: true` flag
- Users must explicitly opt-in to install prerelease versions

This versioning makes it clear to users which versions are stable and which are experimental.

# Releasing

Releases are driven entirely by `package.json`'s `version` field; nothing is published by hand.

## Process

1. Add a new version section to `CHANGELOG.md`.
2. Bump `version` in `package.json`. If `package-lock.json` doesn't pick up the change on its own, run `npm install --package-lock-only`.
3. Open a PR with those changes into `master`. `.github/workflows/release-check.yml` builds and packages the extension as a PR check, so a broken package is caught before merge.
4. Once merged, `.github/workflows/release.yml` runs on the push to master. It detects that the version changed, tags the merge commit `vX.Y.Z`, pushes the tag, then builds and publishes that one package to both Open VSX and the Visual Studio Marketplace.
5. Whether that publish goes out as stable or prerelease follows the Versioning Policy above - the workflow sets `preRelease: true` automatically based on whether the minor version is odd or even.

## Manual escape hatch

Pushing a `vX.Y.Z` tag by hand (matching the version already in `package.json`) re-triggers just the build-and-publish half of the workflow, skipping the tagging step - useful for retrying a failed publish without another version bump. The tag must match `package.json`'s current version, or the run fails before touching any secrets.

## Required secrets

- `OPEN_VSX` - Open VSX Registry personal access token
- `VSCE_PAT` - Visual Studio Marketplace personal access token

# NPM Scripts

This project includes several npm scripts for development and building:

## Development Scripts

### `npm run dev`
**Desktop development with watch mode**
- Starts ReScript compiler in watch mode
- Compiles Less styles automatically
- Runs webpack in development watch mode
- Use with VS Code's <kbd>F5</kbd> to launch Extension Development Host

### `npm run dev-web [folder]`
**Web extension development workflow**
- Builds web extension (production mode)
- Starts ReScript, Less, and Webpack watchers
- Launches VS Code Web on `localhost:3000`
- Opens specified folder (defaults to current directory)
- **Usage**: `npm run dev-web` or `npm run dev-web /path/to/project`

## Build Scripts

### `npm run build`
**Production build for both desktop and web**
- Compiles ReScript to JavaScript
- Compiles Less styles to CSS
- Creates production webpack bundles for both desktop and web targets
- Outputs: `dist/app.bundle.js` (desktop) and `dist/web.bundle.js` (web)

### `npm run clean`
**Clean build artifacts**
- Removes ReScript compilation artifacts
- Use when you need a fresh build

## Publishing Scripts

### `npm run dry-run-publish`
**Pre-publishing build and dependency check**
- Runs the production build
- Lists all production dependencies that will be packaged
- Helps verify what gets included in the published extension
- Used by both `release-check.yml` (as a PR check) and `release.yml` (before
  publishing) - see "Releasing" above

## Testing Scripts

### `npm run test`
**Run test suite**
- Executes project test suite
- Tests are written in ReScript using the test framework

#### Focused test runs

`npm test` runs the full suite. To load only matching compiled test files, set
`AGDA_TEST_GLOB`:

```bash
AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test
AGDA_TEST_GLOB="Connection/Test__Connection__Config*.js" npm test
```

The glob is relative to `lib/js/test/tests` after ReScript compilation.
Selecting one file is useful for fast integration-test iteration because each
`npm test` invocation launches a fresh VS Code Extension Host.

This is file-level selection before Mocha starts. It is different from Mocha
helpers like `it_only` / `describe_only`: those are useful for temporary local
debugging inside a test file, but they require editing source and should not be
committed. `AGDA_TEST_GLOB` keeps source unchanged and controls which test
files are added to Mocha.

#### Isolated VSCodeVim profile

Tests that exercise VSCodeVim use `AGDA_TEST_VIM=on`. The test runner installs
the pinned `vscodevim.vim@1.32.4` release with the VS Code test CLI, caches it
under the ignored `.vscode-test` directory, and loads it with fresh temporary
user data. It does not use or modify extensions installed in your normal VS
Code profile.

Use `AGDA_TEST_VIM=off` for the corresponding control run. Any other value is
rejected. If the variable is omitted, VSCodeVim-specific tests are skipped.

On macOS, Linux, and CI shells:

```bash
AGDA_TEST_VIM=on AGDA_TEST_GLOB="Test__Issue328*.js" npm test
AGDA_TEST_VIM=off AGDA_TEST_GLOB="Test__Issue328*.js" npm test
```

On PowerShell:

```powershell
$env:AGDA_TEST_VIM="on"
$env:AGDA_TEST_GLOB="Test__Issue328*.js"
npm test
```
