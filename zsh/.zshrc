# ==========
# Oh My Zsh?
# ==========

if [[ -s ~/.oh-my-zsh/oh-my-zsh.sh ]]
then source ~/.zsh/oh-my.zsh
else source ~/.zsh/regular.zsh
fi

# =========
# Functions
# =========

# Path Zsh searches for functions
fpath=($fpath ~/.zsh/functions)

autoload term_colors

# =======
# Aliases
# ======

# Macvim
if [[ "`uname`" = Darwin ]]
then
	alias vim="/Applications/MacVim.app/Contents/MacOS/Vim"
fi

# Commands where correction just gets irritating
for cmd in "cp mv mkdir tmux rbenv ncmpcpp subl powder bundle vundle pip which"
do
	alias $cmd="nocorrect $cmd"
done

alias addrepo="sudo add-apt-repository" # add-apt-repository is just too verbose
alias serve="python -m SimpleHTTPServer 8060" # Serve current directory, thanks Python
alias be="bundle exec"

alias weather="weatherman"
alias hastier="cat $1 | haste | pbcopy"
alias mman="middleman"

echo "Did you update your system today?"

