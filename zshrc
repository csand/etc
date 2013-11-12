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

# Override command for headrefs when completing git checkout
zstyle :completion::complete:git-checkout:argument-rest:headrefs command "git for-each-ref --format='%(refname)' refs/heads 2>/dev/null"

# When using git, HEAD^ conflicts with EXTENDED_GLOB, NOMATCH will send it on
# to git correctly

unsetopt NOMATCH

# Undo Prezto override

alias rm='rm'

# Aliases

alias hastier="cat $1 | haste | pbcopy"
alias weechat="weechat-curses"
alias gcd='cd $(git-root)'
alias gtool='PYTHONPATH="/Users/sam/gazaro/b2b" python -m gtool'
