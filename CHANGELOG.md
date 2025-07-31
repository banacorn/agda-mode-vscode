# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)

## v0.6.3 - 2025-07-31

### Added
- Experimental support for VS Code for the Web (GitHub Codespaces, vscode.dev)

### Fixed
- #249: Bugs when giving twice in a row without reloading

## v0.6.2 - 2025-07-19

### Changed
- Replaced Node.js filesystem operations with VSCode FileSystem API for web compatibility
- Release on EVEN minor versions, pre-release on ODD minor versions
- Dev mode flag in settings

### Fixed
- #239: Holes in commented out code (v0.6.1)
- #240: "Next goal" doesn't move the view
- #241: Giving an expression with two question marks is broken 
- #242: No syntax highlighting until first edit 
- #243: Background highlighting does not reevaluate upon loading file with C-c C-l as it should 
- #245: Can't toggle Agda comments under markup comments in literate Agda files by [@ncfavier](https://github.com/ncfavier)
- #246: .lagda.{md,tex}: recognise Agda blocks under comments by [@ncfavier](https://github.com/ncfavier)
- Windows filepath normalization issues in Registry that caused test failures
- Mock Agda executable creation on Windows

## v0.6.1 - 2025-07-04

### Changed
- CI: Cache VS Code & NPM dependencies

### Added
- Better UI for managing connection/installation of Agda/ALS:
    - Display popup message on the result of the download lastest ALS request
    - Display and provide the latest ALS for download
    - Display connection targets in the download folder as installations
    - Display agda/als in PATH as installed targets

### Fixed
- #236: Refine keeps inserting new holes 
- Invalidate cache of ALS release manifest when it does not exist
- Settings `agdaMode.highlighting.IPC`

## v0.6.0 - 2025-06-23

### Changed
- New algorithm for adjusting token highlightings on document changes
- Source goal positions from highlighting information from Agda instead of parsing them with our crappy parser
- Revamped goal/hole management

### Added
- More testings for:
    - `agda-mode.next-goal`
    - `agda-mode.previous-goal`
    - `agda-mode.refine`
    - `agda-mode.helper-function-type`
    - `agda-mode.infer-type`
    - `agda-mode.context`
    - `agda-mode.goal-type`
    - `agda-mode.goal-type-and-context`
    - `agda-mode.goal-type-context-and-inferred-type`
    - `agda-mode.goal-type-context-and-checked-type`
    - `agda-mode.module-contents`
    - `agda-mode.why-in-scope`

### Fixed
- Highlighting & Go-to-definition
- #157: Nested holes are not highlighted properly
- #159: Holes spanning multiple lines are not handled (in literate files)
- #211: Nested comments confuse the parser
- #214: Too permissible hole matching in literate markdown files
- #222: How is the Agda Syntax implemented in agda-mode-vscode?
- #229: Refining a hole can modify other tokens containing the character '?'
- #230: Upgrade @datastructures-js/binary-search-tree to v5.3.2 by [@andy0130tw](https://github.com/andy0130tw)
- #231: Markdown code blocks without agda identifier by [@fredrik-bakke](https://github.com/fredrik-bakke)
- #232: Revert "agda mandatory for md" by [@fredrik-bakke](https://github.com/fredrik-bakke)
- #233: Error message `a.getOr(...).filter is not a function` when I load an Agda file
- #235: Fix character offset when jumping to error by [@ncfavier](https://github.com/ncfavier)

## v0.5.7 - 2025-05-15

### Fixed
- #180: agda-mode suppresses implicit markdown support from vscode by [@fredrik-bakke](https://github.com/fredrik-bakke)
- #184: Workaround for "Deactivation of latex-input?" by [@fredrik-bakke](https://github.com/fredrik-bakke)
- #225: Add idiom brackets to language configuration by [@fredrik-bakke](https://github.com/fredrik-bakke)
- #226: Agda as an embedded language by [@fredrik-bakke](https://github.com/fredrik-bakke)
- #227: Make spellcheck ignore package-lock.json by [@fredrik-bakke](https://github.com/fredrik-bakke)

## v0.5.6 - 2025-04-23

### Added
- #221: [ misc ] setup codespell workflow by [@dannypsnl](https://github.com/dannypsnl)

### Fixed
- #184: Deactivation of latex-input? #184
- #186: can't type into the normalize expression's text input  
- #144: Most of the times input field for any command does not work 
- #117: Allow numeric input to complete ambiguous keyboard shortcuts by [@vic0103520](https://github.com/vic0103520)
- #220: ALS expected behaviour?

### Removed
- Experimental option for ALS connection from the settings

## v0.5.5 - 2025-04-04

### Fixed
- #180: Highlighting breaks on pretty much any edit
- #204: Bad state after C-c C-s (solve)

## v0.5.4 - 2025-03-19

### Added
- New UI for switching between Agda/ALS installations
- New UI for downloading prebuilt ALS from GitHub
- Testings for Agda/ALS connection on all supported platforms

### Changed 
- Drop support for connecting with ALS through TCP temporarily

### Fixed
- #217: Add Forester language support by [@fredrik-bakke](https://github.com/fredrik-bakke)
- #212: [ fix ] update Interval format for upcoming 2.8.0 by [@ncfavier](https://github.com/ncfavier)
- #210: [ fix ] Don't add padding to goal brackets by [@ncfavier](https://github.com/ncfavier)
- #209: agda-mode should work offline
- #208: Basic lagda.typ support by [@mzhang28](https://github.com/mzhang28)
- #206: Update README.md to reflect the changes made in bea7cbe by [@tsung-ju](https://github.com/tsung-ju)
- #200: Fix year in CHANGELOG.md by [@fredrik-bakke](https://github.com/fredrik-bakke)

## v0.5.1 - 2024-11-28

### Fixed
- #202: Pressing Enter in Compute normal form input field opens the current file in another tab every time
- #200: Fix year in CHANGELOG.md by [@fredrik-bakke](https://github.com/fredrik-bakke)
- #199: Fix '\n' in panel and refinement output by [@jiangsy](https://github.com/jiangsy)
- #198: Address several warnings during build by [@jiangsy](https://github.com/jiangsy)
- #197: When reopening a folder, the panel created by Agda-mode is restored as an empty window #197
- #178: \n has started appearing in messages #178 by [@jiangsy](https://github.com/jiangsy)
- #158: C-c C-s and C-c C-a inserts \n instead of newlines by [@jiangsy](https://github.com/jiangsy)

### Changed 
- Deprecated the legacy ReScript Belt & JS modules
- CI: Speedup and reduce cache size by caching Agda artefacts instead of the whole build directory
- CI: Add Agda-2.6.3 to the mix

## v0.5.0 - 2024-11-13

### Fixed
- #196: Improve the logic related to buffer's font size adjustment by [@jiangsy](https://github.com/jiangsy)
- #195: Remove all usage of deprecated api @bs.send.pipe by [@jiangsy](https://github.com/jiangsy)
- #191: Add more detailed splitting command description by [@ChAoSUnItY](https://github.com/ChAoSUnItY)
- #181: Use multi-chord shortcuts to match Emacs #181
- #169: Custom Agda buffer font size in the extension's setting by [@vic0103520](https://github.com/vic0103520)
- #47: Ctrl-X doesn't work as cut with agda-mode

### Changed
- Upgrade ReScript to v11
- Deprecated "reason-promise" in favor of the new async/await syntax in ReScript
- CI overhaul: allow testings be be conducted on all major platforms (Windows, macOS, Ubuntu) and on multiple versions of Agda
- All keybindings of commands are the same as the ones in Emacs

### Added
- More testings for some of the commands

## v0.4.7 - 2023-12-16

### Changed
- Fetch the latest release of Agda Language Server from GitHub

### Fixed
- #172: "Connection Error: Unable to find Agda Language Server" Error downloading language server?
- #176: Many Unicode input sequences no longer work by [@szumixie](https://github.com/szumixie)
- #177: Update `asset/keymap.js` by [@szumixie](https://github.com/szumixie)

## v0.4.6 - 2023-12-09

### Fixed
- Remove `rescript-nodejs` from dependencies to fix the polyfill issue

## v0.4.5 - 2023-12-09

### Changed
- #126: Update language-server-mule to 0.2.4 (Fixes LSP) by [@jul1u5](https://github.com/jul1u5)
- Replaced [@glennsl/bs-json](https://github.com/glennsl/bs-json) with [@glennsl/rescript-json-combinators](rescript-json-combinators)

### Fixed
- #173: `C-c C-r` results in wrong character when applied to character outside of BMP
- #175: Refining a goal having `\` (instead of `λ`) results in an Internal Parse Error
- #125: Modal bindings are not shown in the goal context
- #171: Update `asset/keymap.js` to the latest
- #159: Holes spanning multiple lines are not handled
- Restore highlighting after tokens have been moved around

## v0.4.3 - 2023-12-03

### Changed
- Some adjustments to the UI
- Update ReScript to v10.1.4
- Update language-server-mule to v0.3.0

### Fixed
- Remove unnecessary escape characters from regular expressions & string literal
- Better parsing of display info from Agda EmacsTop

## v0.4.2 - 2023-11-20

### Changed

- #167: Make the font size of Agda buffer the same as editors
- #166: Improve debug buffer: Not printing modules checked and verbosity now

## v0.4.1 - 2023-09-04

### Changed

- #161: Compact UI by [@fredrik-bakke](https://github.com/fredrik-bakke)

### Added

- #163: Define auto indentation rules by [@fredrik-bakke](https://github.com/fredrik-bakke)
- #151: Update language configuration by [@fredrik-bakke](https://github.com/fredrik-bakke)

### Fixed

- #155: Re #79: Disable activating input method inside the search box by [@vic0103520](https://github.com/vic0103520)
- #154: Fix issue #76: Input method is reactivated after entering a backslash… by [@vic0103520](https://github.com/vic0103520)
- #153: Fix issue #117: Allow numeric input to complete ambiguous key bindings by [@vic0103520](https://github.com/vic0103520)

## v0.4.0 - 2023-08-02

The version number v0.4.0 was bumped by accident.
This version is identical to v0.3.13 (except the CHANGELOG and the metadata).

## v0.3.13 - 2023-08-02

### Fixed

- #148: Fix issue #124 by [@lawcho](https://github.com/lawcho)

## v0.3.12 - 2023-05-23

### Added

- #123: Added logo by [@Trebor-Huang](https://github.com/Trebor-Huang)

### Fixed

- #140: Refactor conditions to enable shortcuts [@pimotte](https://github.com/pimotte)
- #121: add lagda.md highlight support by [@choukh](https://github.com/choukh)
- #118: Syntax highlighting for COMPILE and FOREIGN pragmas by [@KislyjKisel](https://github.com/KislyjKisel)

## v0.3.11 - 2022-08-11

### Fixed

- #112: Don't store the agda path in the config by [@ncfavier](https://github.com/ncfavier).
- #114: Fix typos of 'Shortcuts' in documentation by [@pragma-](https://github.com/pragma-).
- #115: [ fix ] "go to definition" on Windows by [@mz71](https://github.com/mz71).

## v0.3.10 - 2022-08-06

### Fixed

- #105: Case Split not working by [@stepchowfun](https://github.com/stepchowfun).
- #109: Remove "machine-overridable" scope from some settings by [@kzvi](https://github.com/kzvi).

## v0.3.9 - 2022-05-05 

### Fixed

- #88: Connection error when requesting the list of goals

## v0.3.8 - 2022-05-02 

### Fixed

- #94: Error: AbstractContextKeyService has been disposed

### Added

- #100: Jump to the middle of goals by [@ncfavier](https://github.com/ncfavier)
- #101: Use user-specified font family by [@plt-amy](https://github.com/plt-amy)

## v0.3.7 - 2021-12-21

### Fixed

- #75: Connection Error: Internal Parse Error
- #81: Command '..' not found
- #82: Panel reloading on multiple commands
- #84: Can't type into the input prompts

## v0.3.6 - 2021-10-29

### Fixed

- #74: Case split not working

## v0.3.5 - 2021-10-25

### Changed

- Save after triggering editor change event for applying highlighting

### Fixed

- Include environment variables when using cached ALS binary

## v0.3.4 - 2021-10-22

### Added

- Allow user to suuply command-line options to both Agda & ALS 
- #72: Feature request: Toggle display of irrelevant arguments

### Fixed

- #73: Prevent Linux distro other than Ubuntu from downloading prebuilt ALS
- #71: Prefer locally installed language server binary over bundled als
- #70: syntax highlighting not working for block comment {- -}

## v0.3.2 - 2021-09-27

### Added

- Option for disabling the Unicode input method 

### Change

- Make Semantic Highlighting (theme colors) the default

### Fixed

- Weird CSS spacing problem when rendering ALS stuff

## v0.3.0 - 2021-09-26

### Added

- Syntax highlighting with theme colors!

### Fixed

- #67: Version changing doesn't seem to work
- #34: Links don't work in the error messages
- #23: Allow to change highlighting colors?
- #19: Highlight stuff using theme colors

## v0.2.18 - 2021-08-30

### Added

- Check and download prebuilt Agda Language Server from GitHub when available

## v0.2.17 - 2021-08-17

### Fixed

- #63: Agda debug does not show up

## v0.2.16 - 2021-08-13

### Added

- #61: `C-u C-x =` command not working

### Changed

- Append instead of flush when displaying RunningInfo

## v0.2.15 - 2021-08-08

### Changed

- Remove dev mode and allow LSP connection via TCP in prod. 

### Fixed

- #60: do not use ctrl+u with terminal focus by [@cspollard](https://github.com/cspollard).

## v0.2.14 - 2021-05-28

### Changed

- Better error message when Agda gets mad

### Fixed

- Reset connection after Agda went mad

## v0.2.13 - 2021-05-27

### Fixed

- #59: After upgraded to 0.2.12, Agda-mode is refuse to load Agda files by C-c C-l
- #57: Option to add command-line flags to agda executable 

## v0.2.12 - 2021-05-26

### Fixed

- #58: Unicode input not triggered on backslash with modifiers 
- #57: Option to add command-line flags to agda executable 
- #56: No "hole" created when theorem contains `--`
- #55: `\asterisk` results in '⁎' but no further options

## v0.2.11 - 2021-03-26

### Added

- #52: Enhancement: Move cursor to first new hole after case split/refine 

### Fixed

- #53: Stuck when the Agda path is wrong 
- #50: Cannot auto focus on the input box 
- View not refreshed when switching between loaded files

## v0.2.10 - 2021-03-25

### Added

- Bracket matching and folding (LSP)
- Lets expressions indent when wrapped (LSP)

## v0.2.9 - 2021-03-14

### Added

- Display connection status on the top right of the panel.

### Fixed

- Goal index prefix "?" went missing.

## v0.2.8 - 2021-03-03

### Added

- Preliminarily support for [Agda Language Server](https://github.com/banacorn/agda-language-server).

### Fixed

- #49: Case split ignores variables when hole has too few spaces
- #47: Ctrl-X doesn't work as cut with agda-mode 
- #45: Cannot copy text from the Agda window
- #44: Goto definition won't work on Windows
- #42: ^C ^{space} chord not working

## v0.2.7 - 2021-01-17

### Fixed

- #43: Load not working after upgrading to v0.2.4

## v0.2.4 - 2021-01-13

### Added

- #39: Switch to different versions of Agda

### Fixed

- #7: Syntax highlighting not working after "wide" symbols in UTF-16
- #41: Split cases without type the variable name, namely just hit the enter key cannot work at the first hit
- #38: can one move the compilation tabs to the right? 

## v0.2.3 - 2020-12-23

### Fixed

- #30: Some input method related issue

### Changed

- Refactor and fortify the unicode symbol input method
- Refactor and remove task queues  

## v0.2.2 - 2020-11-23

### Fixed

- Broken VS Code binding

## v0.2.1 - 2020-11-23

### Fixed

- #33: Syntax highlighting broken on vscode
- #31: Prompt input box too small
- #26: need to reload agda files when navigating back
- #24: single line comment will cause other line turns to grey

## v0.2.0 - 2020-10-08

### Added

- #22: Go to definition! 

### Changed

- QoL UI updates

### Removed

- Prank for [FLOLAC](https://flolac.iis.sinica.edu.tw/)

## v0.1.16 - 2020-09-25

### Change

- Nailed the performance problem of syntax highlighting

## v0.1.15 - 2020-08-31

### Change

- Drastically improve the performance of syntax highlighting

## v0.1.14 - 2020-08-28

### Change

- Improve the performance of highlighting

## v0.1.13 - 2020-08-20

### Change

- Prevent cursor from moving when inside in a goal

## v0.1.12 - 2020-08-18

### Change

- Improve the performance of highlighting

## v0.1.11 - 2020-08-18

### Change

- Highlight stuff only after displaying goals 

## v0.1.10 - 2020-08-17

### Fixed

- Typo when parsing annotations regarding unsolvedmetas

## v0.1.9 - 2020-08-16

### Fixed

- #21: Cut, copy, and paste not working on Windows and Linux

## v0.1.8 - 2020-08-16

### Added

- Prank for FLOLAC

### Fixed

- #20: Failed to display computed normal forms

## v0.1.7 - 2020-08-11

### Change

- Panel view overhaul

### Fixed

- #18: Spaces are not allowed in the path of Agda

## v0.1.6 - 2020-07-27

### Added

- Unicode input method in the input prompt!

### Fixed

- #2: Unicode input not working in the input prompt

## v0.1.5 - 2020-07-27

### Fixed

- #17: Panel cannot display anything

## v0.1.4 - 2020-07-27

### Fixed

- #16: Case split fails in a pattern lambda

### Change

- Input method can be activated by hitting "\" without having to load the Agda file first
- Input method can be deactivated by hitting "escape"

## v0.1.3 - 2020-07-22

### Change

- Restore the cursor position after messing with the holes 

## v0.1.2 - 2020-07-20

### Fixed

- #15: No interaction in lagda.rst files

## v0.1.1 - 2020-07-17

### Fixed

- #10: Casing on variable in lambda produces invalid program text
- #13: Arrow keys don't work after typing backslash

## v0.1.0 - 2020-07-14

### Added

- Command for `agda-mode:restart`.

### Changed

- Improved command titles by [@jonaprieto](https://github.com/jonaprieto).
- Allow more types of files to be loaded by [@jonaprieto](https://github.com/jonaprieto).

### Fixed

- Weird behaviour after switching between tabs.

## v0.0.11 - 2020-07-10

Can't really remember what happened before this, but thanks for the pull requests from [@EdNutting](https://github.com/EdNutting).
