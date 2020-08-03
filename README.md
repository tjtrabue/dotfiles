# dotfiles

What started out as my own personal repo for my zsh dotfiles (used in conjunction with oh-my-zsh)
is continually growing into an entire automation project of its own. It now has scripts/aliases for
bash as well as zsh, and I plan on adding Microsoft PowerShell support in the future as well. I
borrowed heavily from mathiasbynens/dotfiles when structuring this project, and used many of his
scripts (which I edited to suit my needs).

## IMPORTANT NOTE

This project is NOT ready for prime time yet, and I do not recommend you run the install script.
This whole thing is still under active development and I have not created a release branch yet. In
the future, `master` will contain only stable commits tagged for release, and `develop` will be
the branch for active development.

## Configuration Instructions

### Use Awesome Terminal Fonts in oh-my-zsh Powerlevel9k prompt

#### OS X with iTerm2

Installing gabrielelana's Awesome Terminal Fonts is not exactly a walk in the park, and could do
with some more detailed instructions on the project's README. To get one of these fonts working
in your oh-my-zsh theme, do the following:

1. Navigate to the `awesome-terminal-fonts` project,
2. Switch to the `patching-strategy` branch by running `git checkout patching-strategy`,
3. In a Finder window, open the `patched` subdirectory and install any fonts you would like by
double-clicking on them.
4. In iTerm, navigate to ***Preferrences... > Profiles > [your profile] > Text > Change Font***
5. Find one of the patched fonts that you just installed, and select that font for BOTH the
***Font*** and ***Non-ASCII Font*** options.
6. In your `~/.zshrc`, find the line that specifies `ZSH_THEME`, and right ***above*** that line
paste the following: `POWERLEVEL9K_MODE='awesome-fontconfig'`
