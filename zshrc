#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Set additional directories for functions
fpath=(
  $HOME/etc/prompts
  $HOME/etc/funcs
  $fpath
)

# Source dienv if available
if which direnv &>/dev/null; then
  eval "$(direnv hook zsh)"
fi

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# Override command for headrefs when completing git checkout
zstyle :completion::complete:git-checkout:argument-rest:headrefs command "git for-each-ref --format='%(refname)' refs/heads 2>/dev/null"

# When using git, HEAD^ conflicts with EXTENDED_GLOB, NOMATCH will send it on
# to git correctly

unsetopt NOMATCH

# Undo Prezto override

alias rm='rm'

# Autoload functions
autoload -Uz in-progress

# Aliases

alias hastier="cat $1 | haste | pbcopy"
alias gcd='cd $(git-root)'
alias gtool='PYTHONPATH="/Users/sam/gazaro/b2b" python -m gtool'
alias npm-exec='nocorrect PATH=$(npm bin):$PATH'
alias firefox="$HOME/Applications/Firefox.app/Contents/MacOS/firefox"
alias desertbus="livestreamer twitch.tv/desertbus"
alias cl=clear
alias l=ls
alias gbv="git branch -v"
alias use-mongo="autossh -M 30000 -L 27017:localhost:27017 -L 27018:localhost:27018 -L 11211:localhost:11211 -N"

# Keybindings
bindkey 'OA' up-line-or-search
bindkey 'OB' down-line-or-search
