#!/usr/bin/env zsh

setopt extended_glob

DOTDIR="${DOTDIR:-$HOME}"
BINDIR="${BINDIR:-$HOME/bin}"

# ln specifics are OS dependent
ln_cmd='ln -sfv'

# Link dotfiles
for dotfile in $PWD/^(README.md|setup.zsh|teardown.zsh|scripts|*.template); do
  cmd="$ln_cmd $dotfile $DOTDIR/.${dotfile:t}"
  # Prevent dotdir inception, -h checks symlinks
  if [[ ! -h $DOTDIR/.${dotfile:t} ]]
    then eval "$cmd"
    else echo "Skipping $dotfile, $DOTDIR/.${dotfile:t} exists"
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

# Clone NeoBundle and install vim plugins
if [[ ! -d $DOTDIR/.vim/bundle/neobundle.vim ]]; then
  mkdir -p $DOTDIR/.vim/bundle
  git clone git://github.com/Shougo/neobundle.vim.git $DOTDIR/.vim/bundle/neobundle.vim
  vim +NeoBundleCheck +qall
fi

# Link weechat irc conf (contains passwords)
if [[ -d $HOME/Dropbox ]]; then
  eval "$ln_cmd $HOME/Dropbox/Dotfiles/weechat_irc.conf $DOTDIR/.weechat/irc.conf"
fi
