# ==========
# Oh My Zsh!
# ==========

ZSH=$HOME/.oh-my-zsh
ZSH_THEME="coarsesand"
DISABLE_AUTO_UPDATE="true"

plugins=(bundler django git github mercurial node pip powder rake rbenv vundle)
if [[ `uname` == 'Darwin' ]]
  then plugins=(brew $plugins)
fi

source $ZSH/oh-my-zsh.sh
