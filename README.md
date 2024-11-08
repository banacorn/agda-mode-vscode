# agda-mode on VS Code

## Installation

If you have [Agda](https://agda.readthedocs.io/en/latest/getting-started/installation.html) properly installed (to check this in your terminal, type `agda` and see if it's on your machine). Open an Agda file and you should be able to load it by typing <kbd>C-c</kbd> <kbd>C-l</kbd>.

It's okay if you don't have Agda installed, just proceed to the next section and check out our experimental language server for Agda.

## [Agda Language Server](https://github.com/banacorn/agda-language-server)

Simply enable `agdaMode.connection.agdaLanguageServer` in the settings, and then hit *restart* <kbd>C-x</kbd> <kbd>C-r</kbd>.

The language server should be downloaded and installed within seconds.

<img width="658" alt="截圖 2021-08-30 下午10 22 29" src="https://user-images.githubusercontent.com/797844/131354393-6a7e2a39-ae3a-4b1c-b7c9-9e89c283407e.png">

Prebuilt binaries for the language server are available on Windows, macOS, and Ubuntu.

### Extension Activation

To activate the extension, open an Agda file, and trigger either of these 2 commands:

| Command                       |            Keymap             |
| :---------------------------- | :---------------------------: |
| load                          | <kbd>C-c</kbd> <kbd>C-l</kbd> |
| activate unicode input method |         <kbd>\\</kbd>         |

See the next section for the exhaustive list of other commands.
You will get `command ... not found` if you try to trigger other commands without activating the extension first.

## Commands

* <kbd>C-c</kbd> stands for "press <kbd>Ctrl</kbd> and <kbd>c</kbd> at the same time"
* When it comes to combos like <kbd>C-c</kbd> <kbd>C-l</kbd>, you can often slur
them into "hold <kbd>Ctrl</kbd> while pressing <kbd>c</kbd> and then <kbd>l</kbd>"

Commands working with types (marked with the 🎚 emoji below) can have different levels of normalization. 
Prefix commands with `C-u`, `C-u C-u`, or `C-u C-u  C-u` to change the level of normalization or rewrite like in Emacs.

### Global commands

| Command                                 |            Keymap             |
| :-------------------------------------- | :---------------------------: |
| load                                    | <kbd>C-c</kbd> <kbd>C-l</kbd> |
| compile                                 | <kbd>C-x</kbd> <kbd>C-c</kbd> |
| quit                                    | <kbd>C-c</kbd> <kbd>C-q</kbd> |
| quit and restart                        | <kbd>C-x</kbd> <kbd>C-r</kbd> |
| toggle display of hidden arguments      | <kbd>C-x</kbd> <kbd>C-h</kbd> |
| toggle display of irrelevant arguments  | <kbd>C-x</kbd> <kbd>C-i</kbd> |
| show constraints                        | <kbd>C-c</kbd> <kbd>C-=</kbd> |
| solve constraints 🎚                     | <kbd>C-c</kbd> <kbd>C-s</kbd> |
| show all goals 🎚                        | <kbd>C-c</kbd> <kbd>C-?</kbd> |
| move to next goal (forward)             | <kbd>C-c</kbd> <kbd>C-f</kbd> |
| move to previous goal (backwards)       | <kbd>C-c</kbd> <kbd>C-b</kbd> |
| infer type 🎚                            | <kbd>C-c</kbd> <kbd>C-d</kbd> |
| module contents 🎚                       | <kbd>C-c</kbd> <kbd>C-o</kbd> |
| search definitions in scope 🎚           | <kbd>C-c</kbd> <kbd>C-z</kbd> |
| compute normal form 🎚                  | <kbd>C-c</kbd> <kbd>C-n</kbd> |
| switch to a different Agda version      | <kbd>C-x</kbd> <kbd>C-s</kbd> |
| Unicode symbol input sequences lookup   | <kbd>C-x</kbd> <kbd>C-=</kbd> |

### Commands in context of a goal

| Command                                 |             Keymap              |
| :-------------------------------------- | :-----------------------------: |
| give (fill goal)                        | <kbd>C-c</kbd> <kbd>C-SPC</kbd> |
| refine                                  |  <kbd>C-c</kbd> <kbd>C-r</kbd>  |
| elaborate and give 🎚                    |  <kbd>C-c</kbd> <kbd>C-m</kbd>  |
| auto 🎚                                  |  <kbd>C-c</kbd> <kbd>C-a</kbd>  |
| case split                              |  <kbd>C-c</kbd> <kbd>C-c</kbd>  |
| compute helper function type and copy 🎚 |  <kbd>C-y</kbd> <kbd>C-h</kbd>  |
| goal type 🎚                             |  <kbd>C-c</kbd> <kbd>C-t</kbd>  |
| context (environment) 🎚                 |  <kbd>C-c</kbd> <kbd>C-e</kbd>  |
| infer type 🎚                            |  <kbd>C-c</kbd> <kbd>C-d</kbd>  |
| goal type and context 🎚                 |  <kbd>C-c</kbd> <kbd>C-,</kbd>  |
| goal type, context and inferred term 🎚  |  <kbd>C-c</kbd> <kbd>C-.</kbd>  |
| goal type, context and checked term  🎚  |  <kbd>C-c</kbd> <kbd>C-;</kbd>  |
| module contents 🎚                       |  <kbd>C-c</kbd> <kbd>C-o</kbd>  |
| compute normal form 🎚                   | <kbd>C-c</kbd> <kbd>C-n</kbd> |
| why in scope                            |  <kbd>C-c</kbd> <kbd>C-w</kbd>  |

### Commands yet to be implemented

| Command                          |            Keymap             |
| :------------------------------- | :---------------------------: |
| abort a command                  | <kbd>C-x</kbd> <kbd>C-a</kbd> |
| remove goals and highlighting    | <kbd>C-x</kbd> <kbd>C-d</kbd> |
| comment/uncomment rest of buffer |                               |

## Unicode Input

Pretty much the same like on Emacs.
Press backslash "\\" and you should see a keyboard popping up in the panel, with key suggestions and symbol candidates. Use arrow keys to explore and navigate between the candidates (if there's any).

Unicode input also works in the input prompt, though it's a bit less powerful.

If you are having trouble typing the backslash "\\", you can change it by:
1. Go to "Preferences: Open Keyboard Shortcuts" and configure the keybinding of "Agda: Activate input method" (`agda-mode.input-symbol[Activate]`).
2. Go to "Settings > Agda Mode > Input Method: Activation Key" and replace it with the same keybinding as above.

Cancel `agdaMode.inputMethod.enable` in the settings to disable the input method.

## Syntax Highlighting

Cancel `agdaMode.highlighting.getHighlightWithThemeColors` in the settings if you want to fallback to the old way of highlighting stuff with fixed colors.

## Debug Buffer

Execute `Agda: Open Debug Buffer` in the *Command Palette* to open it.
The number at the end of each message indicates its verbosity.

## Troubleshooting

### Agda files won't load, commands don't work

Please go to "Preferences: Open Keyboard Shortcuts" and see if any extension is fighting for the same key combo.
You're probably a victim of the [*Vim* extension](https://marketplace.visualstudio.com/items?itemName=vscodevim.vim).

### "Give" command not working on macOS

*Give* (<kbd>C-c</kbd> <kbd>C-SPC</kbd>) would trigger "Spotlight" (<kbd>C-SPC</kbd>) on Mac.
Please consider using *Refine* (<kbd>C-c</kbd> <kbd>C-r</kbd>) instead.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md)
