# agda-mode on VS Code

Feedbacks, issues, and PRs all welcome!

## Installation

1. Make sure that you have [agda](https://agda.readthedocs.io/en/latest/getting-started/installation.html) properly installed (check this in your terminal, type `agda` and see if it's on your machine).
2. Open an Agda file and you should be able to load it by typing <kbd>C-c</kbd> <kbd>C-l</kbd>.
<!-- 
## TODO & Known Issues

- [ ] Atomic task scheduler (abort/undo/redo)
- [ ] Goals/Warnings/Errors display overhaul
- [ ] Themeable syntax highlighting
- [ ] Settings and whatnot
- [ ] [Unicode input not working in the input prompt](https://github.com/banacorn/agda-mode-vscode/issues/2) -->

## Commands

* <kbd>C-c</kbd> stands for "press <kbd>Ctrl</kbd> and <kbd>c</kbd> at the same time"
* When it comes to combos like <kbd>C-c</kbd> <kbd>C-l</kbd>, you can often slur
them into "hold <kbd>Ctrl</kbd> while pressing <kbd>c</kbd> and then <kbd>l</kbd>"

Commands working with types (marked with the 🎚 emoji below) can have different levels of normalization. However, due to some [technical limitations](https://github.com/microsoft/vscode/issues/6966), we cannot prefix commands with `C-u` or ` C-u C-u` like in Emacs. Instead, we replace the `C-u C-c` prefix with `C-u` and the `C-u C-u C-c` prefix with `C-y`.

Take *infer type* for example:

| Level of normalization                            | Keymap in VS Code               | Keymap in Emacs |
|:--------------------------------------------------|:-------------------------------:|:-------------------------------:|
| "simplified"   (default)                          | <kbd>C-c</kbd> <kbd>C-d</kbd>   | <kbd>C-c</kbd> <kbd>C-d</kbd>   |
| "instantiated" (without further normalisation)    | <kbd>C-u</kbd> <kbd>C-d</kbd>   | <kbd>C-u</kbd> <kbd>C-c</kbd> <kbd>C-d</kbd>   |
| "normalized"   (fully normalized)                 | <kbd>C-y</kbd> <kbd>C-d</kbd>   | <kbd>C-u</kbd> <kbd>C-u</kbd> <kbd>C-c</kbd>  <kbd>C-d</kbd>   |

### Global commands

| Command                                 | Keymap                          |
|:----------------------------------------|:-------------------------------:|
| load                                    | <kbd>C-c</kbd> <kbd>C-l</kbd>   |
| compile                                 | <kbd>C-x</kbd> <kbd>C-c</kbd>   |
| quit                                    | <kbd>C-c</kbd> <kbd>C-q</kbd>   |
| quit and restart                        | <kbd>C-x</kbd> <kbd>C-r</kbd>   |
| toggle display of hidden arguments      | <kbd>C-x</kbd> <kbd>C-h</kbd>   |
| show constraints                        | <kbd>C-c</kbd> <kbd>C-=</kbd>   |
| solve constraints 🎚                    | <kbd>C-c</kbd> <kbd>C-s</kbd>   |
| show all goals                          | <kbd>C-c</kbd> <kbd>C-?</kbd>   |
| move to next goal (forward)             | <kbd>C-c</kbd> <kbd>C-f</kbd>   |
| move to previous goal (backwards)       | <kbd>C-c</kbd> <kbd>C-b</kbd>   |
| infer type 🎚                           | <kbd>C-c</kbd> <kbd>C-d</kbd>   |
| module contents 🎚                      | <kbd>C-c</kbd> <kbd>C-o</kbd>   |
| search definitions in scope 🎚          | <kbd>C-c</kbd> <kbd>C-z</kbd>   |
| compute normal form (default compute)   | <kbd>C-c</kbd> <kbd>C-n</kbd>   |
| compute normal form (ignore abstract)   | <kbd>C-u</kbd> <kbd>C-n</kbd>   |
| compute normal form (use show instance) | <kbd>C-y</kbd> <kbd>C-n</kbd>   |

### Commands in context of a goal

| Command                                 | Keymap                          |
|:----------------------------------------|:-------------------------------:|
| give (fill goal)                        | <kbd>C-c</kbd> <kbd>C-SPC</kbd> |
| refine                                  | <kbd>C-c</kbd> <kbd>C-r</kbd>   |
| elaborate and give 🎚                   | <kbd>C-c</kbd> <kbd>C-m</kbd>   |
| auto                                    | <kbd>C-c</kbd> <kbd>C-a</kbd>   |
| case split                              | <kbd>C-y</kbd> <kbd>C-c</kbd>   |
| compute helper function type and copy 🎚| <kbd>C-y</kbd> <kbd>C-h</kbd>   |
| goal type 🎚                            | <kbd>C-c</kbd> <kbd>C-t</kbd>   |
| context (environment) 🎚                | <kbd>C-c</kbd> <kbd>C-e</kbd>   |
| infer type 🎚                           | <kbd>C-c</kbd> <kbd>C-d</kbd>   |
| goal type and context 🎚                | <kbd>C-c</kbd> <kbd>C-,</kbd>   |
| goal type, context and inferred term 🎚 | <kbd>C-c</kbd> <kbd>C-.</kbd>   |
| goal type, context and checked term  🎚 | <kbd>C-c</kbd> <kbd>C-;</kbd>   |
| module contents 🎚                      | <kbd>C-c</kbd> <kbd>C-o</kbd>   |
| compute normal form (default compute)   | <kbd>C-c</kbd> <kbd>C-n</kbd>   |
| compute normal form (ignore abstract)   | <kbd>C-u</kbd> <kbd>C-n</kbd>   |
| compute normal form (use show instance) | <kbd>C-y</kbd> <kbd>C-n</kbd>   |
| why in scope                            | <kbd>C-c</kbd> <kbd>C-w</kbd>   |

### Commands yet to be implemented

| Command                                 | Keymap                          |
|:----------------------------------------|:-------------------------------:|
| abort a command                         | <kbd>C-x</kbd> <kbd>C-a</kbd>   |
| remove goals and highlighting           | <kbd>C-x</kbd> <kbd>C-d</kbd>   |
| comment/uncomment rest of buffer        |                                 |
| switch to a different Agda version      |                                 |


## Unicode Input

Pretty much the same like on Emacs. 
Press backslash "\\" (after loaded the file) and you should see a keyboard in the panel, with key suggestions and symbol candidates.

## Troubleshooting

### Agda files won't load, commands don't work

Please open the "Keyboard Shortcuts" tool (<kbd>C-k</kbd> + <kbd>C-s</kbd>) and see if any other extensions are fighting for the same key combinations. 

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md)
