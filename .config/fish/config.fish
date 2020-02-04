source ~/.config/fish/aliases.fish

set -x fish_greeting ''
set -g fish_user_paths $HOME/.cargo/bin $fish_user_paths
set -g fish_user_paths $HOME/.npm-packages/bin $fish_user_paths

set -x EDITOR emacsclient -c
set -x CODE $HOME/code

set nix_script $HOME/.nix-profile/etc/profile.d/nix.sh
test -e $nix_script; and fenv source $nix_script

which thefuck >/dev/null; and thefuck --alias | source
which direnv >/dev/null; and direnv hook fish | source
which starship >/dev/null; and eval (starship init fish)
