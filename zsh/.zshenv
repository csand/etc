EDITOR=vim

# Mac settings
if [[ `uname` = 'Darwin' ]]
then
	PATH=/usr/local/sbin:$PATH
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
VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python

if which virtualenvwrapper_lazy.sh &>/dev/null
then
	source virtualenvwrapper_lazy.sh
fi

# Personal bin dir
PATH=$HOME/bin:$PATH
