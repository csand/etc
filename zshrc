#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Set additional directories for functions
fpath=(
  $HOME/etc/prompts
  $fpath
)

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# When using git, HEAD^ conflicts with EXTENDED_GLOB, NOMATCH will send it on
# to git correctly
unsetopt NOMATCH

# prezto ovverides
alias rm='rm'

# Use MacVim's vim
if [[ "$OSTYPE" == darwin* ]]; then
  MACVIM_VIM="/Applications/MacVim.app/Contents/MacOS/Vim"
  if [[ -x "$HOME${MACVIM_VIM}" ]]; then
    alias vim="$HOME${MACVIM_VIM}"
  else
    if [[ -x "${MACVIM_VIM}" ]]; then
      alias vim="${MACVIM_VIM}"
    fi
  fi
fi

alias hastier="cat $1 | haste | pbcopy"
alias mman="middleman"
alias weechat="weechat-curses"

echo "Did you update your system today?"
