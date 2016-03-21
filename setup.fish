#!/usr/bin/env fish

function safe_to_dest -d "Safely use a command to send a file to a \
    destination if the destination does not already exist." -a cmd src dest
    if not test -e $dest
        eval "$cmd $src $dest"
    else
        echo -s 'Skipping ' (basename $src) ', ' $dest ' exists'
    end
end

set dot_dir
if set -q DOTDIR and test -n $DOTDIR
    set dot_dir $DOTDIR
else
    set dot_dir $HOME
end

set ln_cmd 'ln -sfv'

# Link dotfiles
for dotfile in  $PWD/{dir_colors,gitconfig,gvimrc,hgrc,jshintrc,tmux.conf,vimrc,tmuxp,spacemacs}
    set dest $dot_dir/.(basename $dotfile)
    safe_to_dest $ln_cmd $dotfile $dest
end

# Link XDG config files
for dotfile in $PWD/config/*
    set dest $dot_dir/.config/(basename $dotfile)
    safe_to_dest $ln_cmd $dotfile $dest
end

# Copy templates
for template in $PWD/*.template
    set file_name (echo (basename $template) | sed 's/\.template//')
    set dest $dot_dir/.$file_name
    safe_to_dest 'cp' $template $dest
end

# Link scripts into bin directory, creating the dir if necessary
set bin_dir
if set -q BINDIR and test -n $BINDIR
    set bin_dir $BINDIR
else
    set bin_dir $HOME/bin
end

set scripts $PWD/scripts/*
mkdir -p $bin_dir
for script in $PWD/scripts/*
    set dest $bin_dir/(basename $script)
    safe_to_dest $ln_cmd $script $dest
end

# Clone Vundle and install vim plugins
if not test -d $dot_dir/.vim/bundle/vundle
    mkdir -p $dot_dir/.vim/bundle
    git clone git://github.com/gmarik/Vundle.vim.git $dot_dir/.vim/bundle/Vundle.vim
    vim +PluginInstall +qall
end
