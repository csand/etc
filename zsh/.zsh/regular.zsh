# ===============
# Regular ol' Zsh
# ===============

# Completion
autoload -U compinit
compinit

# Prompt themes
autoload -U promptinit
promptinit

# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# Options
setopt autocd
setopt extendedglob
setopt nomatch
setopt appendhistory

unsetopt notify

bindkey -v # Start ZLE in vi insert mode
