function fish_prompt --description "Prompt, based on jekor's"

    # status
    set -l last_status $status
    set -l prompt_status
    if test $last_status -ne 0
        set prompt_status $__fish_prompt_color_bad ✗
    else
        set prompt_status $__fish_prompt_color_good ✓
    end

    # user
    if not set -q __fish_prompt_user
        set -g __fish_prompt_user (whoami)
    end

    # virtualenv
    if set -q VIRTUAL_ENV
        set -gx __fish_prompt_venv (printf '(%s) ' (basename $VIRTUAL_ENV))
    else
        set -gx __fish_prompt_venv ''
    end

    set -l prompt_char λ


    echo -n -s $prompt_status ' ' \
        $__fish_prompt_color_user $__fish_prompt_user ' ' \
        $__fish_prompt_color_cwd (pwd_trunc.py) ' ' \
        (__fish_git_prompt '[%s] ') \
        $__fish_prompt_color_venv $__fish_prompt_venv \
        $__fish_prompt_color_char $prompt_char ' ' (set_color normal)


    # if not set -q __prompt_initialized
    #     set -U __fish_prompt_color_user (set_color green)
    #     set -U __fish_prompt_color_cwd  (set_color $fish_color_cwd)
    #     set -U __fish_prompt_color_venv (set_color magenta)
    #     set -U __fish_prompt_color_char (set_color yellow)
    #     set -U __prompt_initialized
    # end

    set -U __fish_prompt_color_good (set_color $fish_color_user)
    set -U __fish_prompt_color_bad  (set_color $fish_color_error)
    set -U __fish_prompt_color_user (set_color $fish_color_user)
    set -U __fish_prompt_color_cwd  (set_color $fish_color_cwd)
    set -U __fish_prompt_color_venv (set_color magenta)
    set -U __fish_prompt_color_char (set_color $fish_color_quote)
    set -U __prompt_initialized

end
