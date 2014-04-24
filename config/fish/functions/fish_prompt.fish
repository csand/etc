set -g __fish_git_prompt_show_informative_status 1
set -g __fish_git_prompt_hide_untrackedfiles 1

set -g __fish_git_prompt_color_branch magenta bold
set -g __fish_git_prompt_showupstream "informative"
set -g __fish_git_prompt_char_upstream_ahead "↑"
set -g __fish_git_prompt_char_upstream_behind "↓"
set -g __fish_git_prompt_char_upstream_prefix ""

set -g __fish_git_prompt_char_stateseparator ""

set -g __fish_git_prompt_char_stagedstate "●"
set -g __fish_git_prompt_char_dirtystate "+"
set -g __fish_git_prompt_char_untrackedfiles "…"
set -g __fish_git_prompt_char_conflictedstate "✗"
set -g __fish_git_prompt_char_cleanstate ""

set -g __fish_git_prompt_color_dirtystate blue
set -g __fish_git_prompt_color_stagedstate yellow
set -g __fish_git_prompt_color_invalidstate red
set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
set -g __fish_git_prompt_color_cleanstate green

function fish_prompt --description "Prompt, based on jekor's"
    set last_status $status

    set color_good (set_color green)
    set color_maybe (set_color yellow)
    set color_bad  (set_color red)
    set color_user (set_color green)
    set color_cwd  (set_color blue)
    set color_venv (set_color magenta)
    set color_char (set_color yellow)
    set color_git_branch (set_color cyan)
    set color_git_dirty  (set_color red)
    set color_git_staged (set_color yellow)

    # status
    if test $last_status -eq 0
        set prompt_status "$color_good"✓" "
    else
        set prompt_status "$color_bad"✗" $last_status "
    end

    # user
    set prompt_user $color_user(whoami)' '

    # git
    set prompt_git (__fish_git_prompt '(%s) ')

    # virtualenv
    set prompt_venv ''
    if set -q VIRTUAL_ENV
        set prompt_venv (printf '%s[%s] ' $color_venv (basename $VIRTUAL_ENV))
    end

    set prompt_char "$color_char"λ' '

    echo -n -s $prompt_status $prompt_user \
        $color_cwd(pwd_trunc.py) ' ' $prompt_git \
        $prompt_venv $prompt_char (set_color normal)
end
