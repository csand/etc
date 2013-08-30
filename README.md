# Dotfiles

Don't expect me to keep this README up to date, over 8 months have passed since
I last updated it. I tend to change my dotfiles and the setup scripts on a
whim, though the setup scripts have become relatively stable of late.

## Setup

Setup is very simple, all that needs to be run are these commands. I keep my
dotfiles in `~/etc`, then link them out of there. The setup script requires
**zsh** in order to use extended globbing, but it could probably be adapted for
plain **sh**.

```sh
git clone --recursive git://github.com/csand/dotfiles.git ~/etc
cd ~/etc && ./setup.zsh
```
