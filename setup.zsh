#!/usr/bin/env zsh
setopt EXTENDED_GLOB

DOTDIR="${DOTDIR:-$HOME}"
BINDIR="${BINDIR:-$HOME/bin}"
ln_cmd='ln -sf'

for dotfile in $PWD/^(README.md|setup.sh|scripts); do
  "$ln_cmd $dotfile $DOTDIR/.${dotfile:t}"
done

scripts=($PWD/scripts/*)
mkdir -p "$BINDIR"
"$ln_cmd $scripts $BINDIR"
