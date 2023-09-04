# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)

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
