#!/usr/bin/env zsh

setopt extended_glob

DOTDIR="${DOTDIR:-$HOME}"
BINDIR="${BINDIR:-$HOME/bin}"

# ln specifics are OS dependent
ln_cmd='ln -sfv'

# Link dotfiles
for dotfile in $PWD/^(README.md|setup.zsh|teardown.zsh|scripts|vim|*.template); do
  cmd="$ln_cmd $dotfile $DOTDIR/.${dotfile:t}"
  if [[ -d $dotfile ]]; then
    # Prevent dotdir inception, -h checks symlinks
    if [[ ! -h $DOTDIR/.${dotfile:t} ]]; then eval "$cmd"; fi
  else
    eval "$cmd"
  fi
done

# Copy templates
for template in $PWD/*.template; do
  file=$(basename ${template})
  dest=$HOME/${file%.template}
  if [[ ! -f $dest ]]; then
    cp $template $dest
  else
    echo -n 'Skipping '
    echo -n ${template}
    echo ', target exists'
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
