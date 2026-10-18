# agda-mode on VS Code

## Installation

If you have [Agda](https://agda.readthedocs.io/en/latest/getting-started/installation.html) properly installed (to check this in your terminal, type `agda` and see if it's on your machine). Open an Agda file and you should be able to load it by typing <kbd>C-c</kbd> <kbd>C-l</kbd>.

It's okay if you don't have Agda installed, we can also download and install [Agda Language Server](https://github.com/agda/agda-language-server) for you on Windows, macOS, or Ubuntu.

You can install multiple versions of Agda or ALS by adding more paths to `agdaMode.connection.paths` in the settings, and switch between them with <kbd>C-c</kbd> <kbd>C-r</kbd>.

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

Commands working with terms or types (marked with the 🎚 emoji below) can be prefixed with <kbd>C-u</kbd> to compute without further normalisation, with <kbd>C-u</kbd> <kbd>C-u</kbd> to compute normal forms, and <kbd>C-u</kbd> <kbd>C-u</kbd> <kbd>C-u</kbd> to compute weak-head normal forms like in Emacs.

### Global commands

| Command                                 |            Keymap             |
| :-------------------------------------- | :---------------------------: |
| load                                    | <kbd>C-c</kbd> <kbd>C-l</kbd> |
| compile                                 | <kbd>C-c</kbd> <kbd>C-x</kbd> <kbd>C-c</kbd> |
| quit                                    | <kbd>C-c</kbd> <kbd>C-x</kbd> <kbd>C-q</kbd> |
| quit and restart                        | <kbd>C-c</kbd> <kbd>C-x</kbd> <kbd>C-r</kbd> |
| toggle display of hidden arguments      | <kbd>C-c</kbd> <kbd>C-x</kbd> <kbd>C-h</kbd> |
| toggle display of irrelevant arguments  | <kbd>C-c</kbd> <kbd>C-x</kbd> <kbd>C-i</kbd> |
| show constraints 🎚[^1]                  | <kbd>C-c</kbd> <kbd>C-=</kbd> |
| solve constraints 🎚                     | <kbd>C-c</kbd> <kbd>C-s</kbd> |
| show all goals 🎚                        | <kbd>C-c</kbd> <kbd>C-?</kbd> |
| move to next goal (forward)             | <kbd>C-c</kbd> <kbd>C-f</kbd> |
| move to previous goal (backwards)       | <kbd>C-c</kbd> <kbd>C-b</kbd> |
| infer type 🎚                            | <kbd>C-c</kbd> <kbd>C-d</kbd> |
| module contents 🎚                       | <kbd>C-c</kbd> <kbd>C-o</kbd> |
| search definitions in scope 🎚           | <kbd>C-c</kbd> <kbd>C-z</kbd> |
| compute normal form 🎚                  | <kbd>C-c</kbd> <kbd>C-n</kbd> |
| switch to a different installation of Agda or ALS | <kbd>C-c</kbd> <kbd>C-x</kbd> <kbd>C-s</kbd> |
| Unicode symbol input sequences lookup   | <kbd>C-x</kbd> <kbd>C-=</kbd> |

[^1]: Normalisation prefixes only have an effect on Agda 2.9.0 and higher for this command.

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

If you are having trouble typing the backslash "\\", go to "Preferences: Open Keyboard Shortcuts" and configure the keybinding of "Agda: Activate input method" (`agda-mode.input-symbol[Activate]`).
By default, this command is bound to VS Code's layout-independent `[Backslash]` scan code.

Cancel `agdaMode.inputMethod.enable` in the settings to disable the input method.

## Highlighting

### Names

Names are colored with VS Code's semantic tokens, so your color theme decides how they look.
Each kind of name gets a token type:

| Name                                            | Token type                          |
| ----------------------------------------------- | ----------------------------------- |
| keywords, comments, strings, numbers            | `keyword`, `comment`, `string`, `number` |
| datatypes                                       | `type`                              |
| records                                         | `class`                             |
| constructors                                    | `enumMember`                        |
| coinductive constructors                        | `enumMember` with `coInductive`     |
| record fields                                   | `property`                          |
| functions                                       | `function`                          |
| postulates                                      | `function` with `abstract`          |
| modules                                         | `namespace`                         |
| bound variables                                 | `variable`                          |
| arguments                                       | `parameter`                         |
| macros                                          | `macro`                             |
| operators                                       | `operator`                          |
| built-in sorts (`Set`, `Prop`) and `primitive` functions | `type` with `primitive`    |

Most tokens also carry the `agda` modifier, so a rule for `*.agda` selects all of them.
To change a color, add a rule to the `editor.semanticTokenColorCustomizations` setting:

```json
"editor.semanticTokenColorCustomizations": {
    "rules": {
        "type.primitive": "#C586C0"
    }
}
```

### Problem markers

Problem markers, such as unsolved metas and termination problems, are drawn as backgrounds and underlines.
Their colors are theme colors named `agdaMode.*`, and you can override them in the `workbench.colorCustomizations` setting:

```json
"workbench.colorCustomizations": {
    "agdaMode.unsolvedMeta": "#FF00FF",
    "agdaMode.deadcode": "#33333388"
}
```

These colors can be overridden.
The defaults follow the faces of Agda's Emacs mode (`agda2-highlight-*-face`).
`agdaMode.error` is the color of the underlined text; the others are background colors.

| Id                             | Light     | Dark      |
| ------------------------------ | --------- | --------- |
| `agdaMode.error`               | `#FF0000` | `#FF0000` |
| `agdaMode.errorWarning`        | `#F08080` | `#802400` |
| `agdaMode.unsolvedMeta`        | `#FFFF00` | `#806B00` |
| `agdaMode.unsolvedConstraint`  | `#FFFF00` | `#806B00` |
| `agdaMode.terminationProblem`  | `#FFA07A` | `#802400` |
| `agdaMode.positivityProblem`   | `#CD853F` | `#803F00` |
| `agdaMode.deadcode`            | `#A9A9A9` | `#808080` |
| `agdaMode.shadowingInTelescope`| `#A9A9A9` | `#808080` |
| `agdaMode.coverageProblem`     | `#F5DEB3` | `#805300` |
| `agdaMode.incompletePattern`   | `#800080` | `#800080` |
| `agdaMode.missingDefinition`   | `#FFA500` | `#804040` |
| `agdaMode.instanceProblem`     | `#F5DEB3` | `#805300` |
| `agdaMode.cosmeticProblem`     | `#F5F5F5` | `#404040` |
| `agdaMode.catchallClause`      | `#F5F5F5` | `#404040` |
| `agdaMode.confluenceProblem`   | `#FFC0CB` | `#800080` |

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
