# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# disable C-q and C-s
# from http://unix.stackexchange.com/a/12108
stty -ixon

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# add /usr/local/bin to path:
export PATH=$PATH:/usr/local/bin

# 256 terminal colours
if [ -n "$DISPLAY" -a "$TERM" == "xterm" ]; then
    export TERM=xterm-256color
fi
