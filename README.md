# Dotfiles

## Description

All of my dotfiles, stored in a format suitable for use with [GNU stow][stow].
[Vundle][vundle] and [Oh My ZSH][ohmyzsh] are included as submodules that will
be symlinked into the correct locations by stow (<code>~/.vim/bundle/vundle
</code> and <code>~/.oh-my-zsh</code>.)

## Usage

Obviously, you'll have to have [stow][stow] installed. It's a pretty useful
tool, so go grab it.

	stow -t $HOME _package_

Will link the tree in <code>_package_</code> into your home directory. Useful
if you only want to use one or two of the packages. **Special note**, the
<code>scripts</code> package is meant to be linked to a folder on your
<code>PATH </code>. In my case I use either <code>~/bin</code> or
<code>~/.bin</code> for personal scripts, so <code>stow -t $HOME/bin
scripts</code> will link them correctly.

	setup.sh

Uses stow to link every package in the repository in the correct order and to
the correct location. Not very complicated, just a list of stow commands useful
for initial setup.

## Thanks

It's a reasonable guess that 90% of my dotfiles were inspired by, or copied
directly from, [Steve Losh][sjl-blog], either from his [oh-my-zsh theme]
[sjl-ohmyzsh], or [his .vimrc][sjl-vimrc], or the excellent [Twitter account]
[sjl-twitter] he runs.

[stow]: http://www.gnu.org/software/stow/
[vundle]: http://github.com/gmarik/vundle
[ohmyzsh]: http://github.com/robbyrussell/oh-my-zsh
[sjl-blog]: http://stevelosh.com/
[sjl-ohmyzsh]: http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/
[sjl-vimrc]: http://stevelosh.com/blog/2010/09/coming-home-to-vim/
[sjl-twitter]: http://twitter.com/dotvimrc

