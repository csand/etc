function with_range
    for i in $argv
        echo $i
    end
end

function fish_prompt --description "Prompt, based on jekor's"

    set -g ___fish_git_prompt_color_dirtystate_done ' '
    set -g ___fish_git_prompt_char_dirtystate  'dirty'
    set -g ___fish_git_prompt_char_stagedstate 'staged'

    set color_good (set_color green)
    set color_bad  (set_color red)
    set color_user (set_color green)
    set color_cwd  (set_color blue)
    set color_venv (set_color magenta)
    set color_char (set_color yellow --bold)
    set color_git_branch (set_color cyan)
    set color_git_dirty  (set_color red)
    set color_git_staged (set_color yellow)

    # status
    set last_status $status
    if test $last_status -ne 0
        set prompt_status "$color_bad"✗
    else
        set prompt_status "$color_good"✓
    end
    set prompt_status $prompt_status' '

    # user
    set prompt_user $color_user(whoami)' '

    # git
    set -gx __fish_git_prompt_showdirtystate 1
    set _git_prompt (with_range (__fish_git_prompt '%s'))
    # echo $_git_prompt[1]
    set prompt_branch $_git_prompt[1]
    switch _git_prompt[2]
        case 'dirty'
            set prompt_branch $color_git_dirty$prompt_branch
        case 'staged'
            set prompt_branch $color_git_staged$prompt_branch
        case '*'
            set prompt_branch $color_git_branch$prompt_branch
    end

    # virtualenv
    set prompt_venv ''
    if set -q VIRTUAL_ENV
        set prompt_venv (printf "$color_venv(%s) " (basename $VIRTUAL_ENV))
    end

    set prompt_char "$color_char"λ' '

    echo -n -s $prompt_status $prompt_user \
        $color_cwd(pwd_trunc.py) ' ' (__fish_git_prompt '[%s] ') \
        $prompt_venv $prompt_char (set_color normal)
end
