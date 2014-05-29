#
# Fish
#

set -e fish_greeting

#
# Browser
#

set -gx BROWSER open

#
# Editor
#

set -gx EDITOR vim
set -gx VISUAL vim
set -gx PAGER less

#
# Projects Directory
#

set -gx PROJECTS $HOME/code

#
# PATH
#

set -gx PATH /{bin,sbin} $PATH
set -gx PATH /usr/{bin,sbin} $PATH
set -gx PATH /usr/local/{bin,sbin} $PATH
set -gx PATH /usr/local/opt/ruby/bin $PATH
set -gx PATH /usr/local/coreutils/libexec/gnubin $PATH
set -gx PATH $HOME/bin $PATH

#
# TERM
#

set -gx TERM xterm-256color

#
# Python
#

set -gx WORKON_HOME $HOME/.virtualenvs
set -gx PIP_VIRTUALENV_BASE $WORKON_HOME
set -gx PIP_RESPECT_VIRTUALENV true
set -gx VIRTUAL_ENV_DISABLE_PROMPT true

#
# Secret ENVVARs
#

test -e $HOME/.secretenv.fish
and source $HOME/.secretenv.fish

#
# Android SDK
#

set -gx ANDROID_HOME /usr/local/opt/android-sdk

#
# Git Prompt
#

set -gx __fish_git_prompt_color_prefix cyan
set -gx __fish_git_prompt_color_suffix cyan

#
# Interactive Settings
#

if status --is-interactive

    # direnv
    eval (direnv hook fish)

    # aliases
    alias l ls
    alias gtool 'python -m gtool'
    alias use-mongo 'autossh -M 30000 -L 27017:localhost:27017 -L 27018:localhost:27018 -L 11211:localhost:11211 -N'
    alias serve 'python -m SimpleHTTPServer'
end
