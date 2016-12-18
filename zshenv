#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

editor='vim'

export EDITOR=$editor
export VISUAL=$editor
export PAGER='less'
export PROJECTS=${HOME}/code

#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

#
# Paths
#

typeset -gU cdpath fpath mailpath path

# Set the list of directories that Zsh searches for programs.
path=(
  $HOME/bin
  $HOME/.cargo/bin
  /usr/local/{bin,sbin}
  /usr/local/share/npm/bin
  /usr/local/opt/ruby/bin
  /usr/local/opt/coreutils/libexec/gnubin
  /usr/{bin,sbin}
  /{bin,sbin}
  $path
)

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

#
# Temporary Files
#

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$USER"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi

#
# Python Virtual Environments
#

export WORKON_HOME="$HOME/.virtualenvs"
export PIP_VIRTUALENV_BASE="$WORKON_HOME"
export PIP_RESPECT_VIRTUALENV=true
export VIRTUAL_ENV_DISABLE_PROMPT=true

if which virtualenvwrapper_lazy.sh &>/dev/null
then
  source virtualenvwrapper_lazy.sh
fi

#
# Secret ENVVARs
#
source $HOME/.secretenv

#
# Active JIRA Issue
#
if [ -f $HOME/.jiraissue ]; then source $HOME/.jiraissue; fi

#
# Android SDK
#
export ANDROID_HOME=/usr/local/opt/android-sdk

#
# Rust + Racer
#
export RUST_SRC_PATH="$HOME/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
