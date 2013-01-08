preexec () {
	echo ''
}

is_root_user () {
  [ $(whoami) = root ]
}

my_git_prompt_info () {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  branch_name="${ref#refs/heads/}"
  echo -n "on branch %{$fg[blue]%}$branch_name%{$reset_color%}"
}

my_standard_prompt_info () {
  user="%{$fg_bold[yellow]%}%n%{$reset_color%}"
  hostname="%{$fg[red]%}%m%{$reset_color%}"
  working_dir="%{$fg_bold[magenta]%}%4~%{$reset_color%}"
  echo -n "$user at $hostname in $working_dir"
}

my_venv_prompt_info () {
  if [ $VIRTUAL_ENV ]
    then
      venv_name=$(basename $VIRTUAL_ENV)
      echo -n "with venv %{$fg_bold[magenta]%}$venv_name%{$reset_color%}"
  fi
}

my_prompt_char () {
  if is_root_user
    then echo -n "%{$fg_bold[red]%}"
    else echo -n "%{$fg_bold[white]%}"
  fi
  echo -n "⇒ %{$reset_color%}"
}

# %m - machine hostname up to first .
# %n - username
# %~ - path relative to ~, number after % is the limit of parents to show

PS1="
  $(my_standard_prompt_info) $(my_git_prompt_info) $(my_venv_prompt_info)
  $(my_prompt_char)"

# ZSH_THEME_GIT_PROMPT_PREFIX=" (%{$fg[red]%}git:%{$reset_color%}%{$fg_bold[yellow]%}"
# ZSH_THEME_GIT_PROMPT_DIRTY="%{$reset_color%}%{$fg[red]%}!"
# ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$reset_color%}%{$fg[red]%}?"
# ZSH_THEME_GIT_PROMPT_CLEAN=""
# ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%})"
