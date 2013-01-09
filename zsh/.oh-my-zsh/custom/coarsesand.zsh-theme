function virtualenv_info {
  if [ $VIRTUAL_ENV ]; then;
    echo "(%{$fg_bold[magenta]%}"`basename %$VIRTUAL_ENV`"%{$reset_color%})";
  fi
}

function precmd {
}

function preexec {
	echo ''
}

ZSH_THEME_GIT_PROMPT_PREFIX=" (%{$fg[red]%}git:%{$reset_color%}%{$fg_bold[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%})"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$reset_color%}%{$fg[red]%}!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$reset_color%}%{$fg[red]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN=""

export PROMPT="
   %{$fg[blue]%}%n@%m:%{$reset_color%}%{$fg[green]%}%~%{$reset_color%} \
$(virtualenv_info)$(git_prompt_info)
   ⇒ "

export RPROMPT=''
