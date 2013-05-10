#!/usr/bin/env zsh

setopt extended_glob

DOTDIR=${DOTDIR:-$HOME}
BINDIR=${BINDIR:-$HOME/bin}

# Unlink dotfiles
for dotfile in $PWD/^(README.md|*.zsh|scripts); do
  rm -v $DOTDIR/.${dotfile:t}
done

# Unlink scripts
scripts=($PWD/scripts/*)
rm -v $BINDIR/${scripts:t}
