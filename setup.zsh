#!/usr/bin/env zsh

setopt extended_glob

DOTDIR="${DOTDIR:-$HOME}"
BINDIR="${BINDIR:-$HOME/bin}"

# ln specifics are OS dependent
ln_cmd='ln -sfv'

# Link dotfiles
for dotfile in $PWD/^(README.md|setup.zsh|teardown.zsh|scripts|config|*.template); do
  cmd="$ln_cmd $dotfile $DOTDIR/.${dotfile:t}"
  # Prevent dotdir inception, -h checks symlinks
  if [[ ! -h $DOTDIR/.${dotfile:t} ]]
    then eval "$cmd"
    else echo "Skipping $dotfile, $DOTDIR/.${dotfile:t} exists"
  fi
done

# Link config files
for dotfile in $PWD/config/*; do
  cmd="$ln_cmd $dotfile $HOME/.config/${dotfile:t}"
  # Prevent dotdir inception, -h checks symlinks
  if [[ ! -h $HOME/.config/${dotfile:t} ]]
    then eval "$cmd"
    else echo "Skipping $dotfile, $HOME/.config/${dotfile:t} exists"
  fi
done

# Copy templates
for template in $PWD/*.template; do
  file=$(basename ${template})
  dest=$HOME/.${file%.template}
  if [[ ! -f $dest ]]; then
    cp $template $dest
  else
    echo -n 'Skipping '
    echo -n ${template}
    echo ', $dest exists'
  fi
done

# Link scripts into bin directory, creating if necessary
scripts=($PWD/scripts/*)
mkdir -p $BINDIR
eval "$ln_cmd $scripts $BINDIR"

# Clone Vundle and install vim plugins
if [[ ! -d $DOTDIR/.vim/bundle/vundle ]]; then
  mkdir -p $DOTDIR/.vim/bundle
  git clone git://github.com/gmarik/vundle.git $DOTDIR/.vim/bundle/vundle
  vim +PluginInstall +qall
fi

# Link weechat irc conf (contains passwords)
if [[ -d $HOME/Dropbox/Dotfiles ]]; then
  for dotfile in $HOME/Dropbox/Dotfiles/*; do
    dest=$(echo ${dotfile:t} | sed 's/_/\//g')
    eval "$ln_cmd $dotfile $DOTDIR/.${dest}"
  done
fi
