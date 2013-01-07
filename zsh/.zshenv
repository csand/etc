
PATH=$HOME/bin:$PATH

# Homebrew
PATH=/usr/local/sbin:$PATH

# Pythons
if [[ -d /usr/local/share/python ]]
  then PATH=/usr/local/share/python:$PATH
fi

if [[ -d /usr/local/share/python3 ]]
  then PATH=/usr/local/share/python3:$PATH
fi

# rbenv
if which rbenv &>/dev/null
  then eval "$(rbenv init -)"
fi

# virtualenvwrapper
WORKON_HOME=$HOME/.virtualenvs/
PIP_VIRTUALENV_BASE=$WORKON_HOME
PIP_RESPECT_VIRTUALENV=true
VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
if which virtualenvwrapper.sh &>/dev/null; then source virtualenvwrapper.sh; fi
