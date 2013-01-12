EDITOR=vim
CLICOLOR=1

# Mac settings
if [[ `uname` = 'Darwin' ]]
then
	LSCOLORS=exBxcxCxbxgxGxdxdxGeEx
	PATH=/usr/local/sbin:$PATH # Homebrew
fi

# Pythons
if [[ -d /usr/local/share/python ]]
then
	PATH=/usr/local/share/python:$PATH
fi

if [[ -d /usr/local/share/python3 ]]
then
	PATH=/usr/local/share/python3:$PATH
fi

# rbenv
# Get it on the path first is ~/.rbenv exists
if [[ -d ~/.rbenv ]]
then
	PATH=$HOME/.rbenv/bin:$PATH
fi

if which rbenv &>/dev/null
then
	eval "$(rbenv init -)"
fi

# virtualenvwrapper
WORKON_HOME=$HOME/.virtualenvs/
PIP_VIRTUALENV_BASE=$WORKON_HOME
PIP_RESPECT_VIRTUALENV=true
VIRTUAL_ENV_DISABLE_PROMPT=true

if which virtualenvwrapper_lazy.sh &>/dev/null
then
	source virtualenvwrapper_lazy.sh
fi

# Personal bin dir
PATH=$HOME/bin:$PATH
