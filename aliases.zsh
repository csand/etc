alias hastier="cat $1 | haste | pbcopy"
alias weechat="weechat-curses"
alias gcd='cd $(git-root)'
alias gtool='PYTHONPATH="/Users/sam/gazaro/b2b" python -m gtool'

# fd - cd to selected directory
fcd() {
  DIR=$(find ${1:-*} -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf) && cd "$DIR"
}

# fda - including hidden directories
fcda() {
  DIR=$(find ${1:-*} -type d 2> /dev/null | fzf) && cd "$DIR"
}

# fh - repeat history
fhist() {
  eval $(history | fzf +s | sed 's/ *[0-9]* *//')
}

# fkill - kill process
fkill() {
  ps -ef | sed 1d | fzf | awk '{print $2}' | xargs kill -${1:-9}
}
