# Dotfiles

## Description

For a long time I kept these synchronized between machines using Dropbox and
symlinks. While that approach does work, Dropbox doesn't cope very well with
moving lots of small files, like those stored in .git directories, one of which
I had for almost every Vim plugin installed. It could take a few hours for
Dropbox to synchronize everything to a new machine, so instead I plan to just
keep the Makefile in Dropbox and let git/vundle do the heavy lifting.

## Usage

<dl>
<dt>`make` or `make links`</dt>
<dd>
Will clone the repository to *~/.files*, symlink all the files and directories
to where they need to go, and also run `:BundleInstall!` in vim in order to
pull down all of the plugins I've setup in my .vimrc.
</dd>
<dt>`make repo`</dt>
<dd>
If you just want to pull down the repo to *~/.files* without creating any of
the links, or installing the Vim plugins. Don't worry, I won't ask why you only
have this Makefile and not the rest of the repo.
</dd>
<dt>`make clean`</dt>
<dd>
Ruin all of those symlinks `make` worked so hard to create. Also useful if
you have other links already set up and need to remove them before running
`make links`. Set to interactive mode so you don't accidentally eat any real
files when you run it.
</dd>
</dl>

## Thanks

To anyone reading this, I'd bet 90% of my dotfiles were inspired by, or copied
directly from, [Steve Losh](http://stevelosh.com/), either from his [oh-my-zsh
theme](http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/), or
[his .vimrc](http://stevelosh.com/blog/2010/09/coming-home-to-vim/), or the
excellent [Twitter account](https://twitter.com/dotvimrc) he runs. Go give [his
blog](http://stevelosh.com/blog/) a read.

