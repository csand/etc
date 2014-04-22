function fish_prompt --description "Prompt, based on jekor's"
    set last_status $status

    set color_good (set_color green)
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
        set prompt_status "$color_bad"✗" $status "
    end

    # user
    set prompt_user $color_user(whoami)' '

    # git
    # force the fish git prompt to give useful output
    # set -g ___fish_git_prompt_color_dirtystate_done ' '
    # set -g ___fish_git_prompt_char_dirtystate  'dirty'
    # set -g ___fish_git_prompt_char_stagedstate 'staged'
    # set -gx __fish_git_prompt_showdirtystate 1

    # set prompt_git ''
    # set _git_prompt (splitline (__fish_git_prompt '%s'))
    # if test -n "$_git_prompt"
    #     set branch $_git_prompt[1]
    #     set _git_status $_git_prompt[2]
    #     if test -n $_git_status
    #         switch $_git_status
    #             case dirty
    #                 set prompt_git (echo -sn (set_color red) $branch)
    #             case staged
    #                 set prompt_git (echo -sn (set_color yellow) $branch)
    #         end
    #     else
    #         set prompt_git $branch
    #     end
    #     set prompt_git (printf '%s[%s%s] ' $color_git_branch $prompt_git $color_git_branch)
    # end

    # virtualenv
    set prompt_venv ''
    if set -q VIRTUAL_ENV
        set prompt_venv (printf "$color_venv(%s) " (basename $VIRTUAL_ENV))
    end

    set prompt_char "$color_char"λ' '

    echo -n -s $prompt_status $prompt_user \
        $color_cwd(pwd_trunc.py) ' ' $prompt_git \
        $prompt_venv $prompt_char (set_color normal)
end
