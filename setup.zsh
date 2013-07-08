#!/usr/bin/env zsh

setopt extended_glob

DOTDIR="${DOTDIR:-$HOME}"
BINDIR="${BINDIR:-$HOME/bin}"

# ln specifics are OS dependent
ln_cmd='ln -sfv'

# Link dotfiles
for dotfile in $PWD/^(README.md|setup.zsh|teardown.zsh|scripts|vim); do
  cmd="$ln_cmd $dotfile $DOTDIR/.${dotfile:t}"
  if [[ $dotfile == 'vim' ]]; then
    # Prevents vim dir inception
    if [[ ! -h $DOTDIR/.vim ]]; then eval "$cmd"; fi
  else
    eval "$cmd"
  fi
done

# Link scripts into bin directory, creating if necessary
scripts=($PWD/scripts/*)
mkdir -p $BINDIR
eval "$ln_cmd $scripts $BINDIR"

# Clone NeoBundle and install vim plugins
if [[ ! -d $DOTDIR/.vim/bundle/neobundle.vim ]]; then
  mkdir -p $DOTDIR/.vim/bundle
  git clone git://github.com/Shougo/neobundle.vim.git $DOTDIR/.vim/bundle/neobundle.vim
  vim +NeoBundleCheck +qall
fi
