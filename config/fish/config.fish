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

set -gx EDITOR 'emacsclient -c'
set -gx VISUAL 'emacsclient -c'
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
set -gx PATH /usr/local/bin $PATH
set -gx PATH /usr/local/opt/ruby/bin $PATH
set -gx PATH $HOME/.cargo/bin $PATH
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
# rbenv
#

set -gx RBENV_ROOT /usr/local/var/rbenv

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
# virtualfish
#

python -m virtualfish | source

#
# Interactive Settings
#

if status --is-interactive
    # direnv
    # Fix an Emacs startup problem
    direnv hook fish | source

    # aliases
    alias l ls
    alias serve 'python -m SimpleHTTPServer'
    alias ql 'qlmanage -p'
    alias die 'exit'

    if which hub > /dev/null
        alias git 'hub'
    end
end

#
#
#

set -gx RUST_SRC_PATH $HOME/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src


#
# Local Settings
#

source ~/.fishlocal
