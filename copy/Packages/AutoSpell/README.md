AutoSpell
=============================
[![Build Status](https://travis-ci.org/wburningham/AutoSpell.svg)](https://travis-ci.org/wburningham/AutoSpell)

Sublime Text 3 packge to auto replace spelling mistakes.

By default the following characters will trigger a correction:

	-:;_,.  and the enter key

Triggers can be added to in `SublimeText -> Preferences -> Package Settings -> AutoSpell -> Settings – User`

You can add to this list of triggers by modifying the `custom_triggers` section. If you want to disable the default triggers, set `default_triggers` to `false`.

By default the package corrects common words and their capitalized versions. For example `teh` and `Teh` will be replaced by `the` and `The`. You can add to this list of replacements by modifying the `custom_replacements` section. If you want to disable the default replacements, set `default_replacements` to `false`.

See the default config for examples.

If you have a list of words that would be good to add to the default list of replacements please submit a pull request.