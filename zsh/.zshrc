# ===
# ZSH
# ===

# Completion

# zstyle ':completion:*' completer _complete _ignored
# zstyle :compinstall filename '/Users/sand/.zshrc'

autoload -Uz compinit
compinit

# History
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

# Environment
EDITOR="subl -w"
CLICOLOR=1
VIRTUAL_ENV_DISABLE_PROMPT=true

# Mac Specific
if [[ "`uname`" = Darwin ]]
then
  LSCOLORS=exBxcxCxbxgxGxdxdxGeEx
fi

# Options
setopt autocd
setopt extendedglob
setopt nomatch
setopt appendhistory

unsetopt notify

# =======
# Aliases
# ======

if [[ "`uname`" != 'Darwin' ]]
  then alias vim="vim"
  else alias vim="/Applications/MacVim.app/Contents/MacOS/Vim"
fi

alias addrepo="sudo add-apt-repository" # add-apt-repository is just too verbose
alias serve="python -m SimpleHTTPServer 8060" # Serve current directory, thanks Python
alias be="bundle exec"

alias weather="weatherman"
alias hastier="cat $1 | haste | pbcopy"
alias mman="middleman"

# ===========
# Keybindings
# ===========

bindkey -v # Start ZLE in vi insert mode

echo "Did you update your system today?"
