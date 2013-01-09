PROMPT='$(prompt_coarsesand)'

prompt_coarsesand () {
  user_name="%{$fg_bold[yellow]%}%n%{$reset_color%}"
  host_name="%{$fg[red]%}%m%{$reset_color%}"
  working_dir="%{$fg_bold[magenta]%}%4~%{$reset_color%}"

  prompt_char=$(my_prompt_char)
  git_branch=$(my_git_branch_info)
  git_status=$(my_git_status_info)
  venv_info=$(my_venv_prompt_info)

  echo -n "
  ${user_name} at ${host_name} in ${working_dir}${git_branch}${git_status}${venv_info}
  ${prompt_char}"
}

# Hooks

preexec () {
	echo ''
}

# Util

is_root_user () {
  [ $(whoami) = root ]
}

# Git

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_DIRTY="!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="?"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"

my_git_branch_info () {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  branch_name="${ref#refs/heads/}"
  echo -n " on branch %{$fg[blue]%}$branch_name%{$reset_color%}"
}

my_git_status_info () {
  echo -n "\
$ZSH_THEME_GIT_PROMPT_PREFIX\
$(parse_git_dirty)\
$ZSH_THEME_GIT_PROMPT_SUFFIX\
"
}

# Virtualenv

my_venv_prompt_info () {
  if [ $VIRTUAL_ENV ]
    then
      venv_name=$(basename $VIRTUAL_ENV)
      echo -n " venv %{$fg_bold[green]%}${venv_name}%{$reset_color%}"
  fi
}

my_prompt_char () {
  if is_root_user
    then echo -n "%{$fg_bold[red]%}"
    else echo -n "%{$fg_bold[white]%}"
  fi
  echo -n "⇒ %{$reset_color%}"
}

