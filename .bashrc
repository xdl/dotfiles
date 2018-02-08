# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# add /usr/local/bin to path:
export PATH=$PATH:/usr/local/bin

# Caveat for brew install python
export PATH="/usr/local/opt/python/libexec/bin:$PATH"


# 256 terminal colours
if [ -n "$DISPLAY" -a "$TERM" == "xterm" ]; then
    export TERM=xterm-256color
fi

# disable C-q and C-s
# from http://unix.stackexchange.com/a/12108
stty -ixon
stty stop undef

source ~/git-completion.bash

# Aliases

alias vlc="/Applications/VLC.app/Contents/MacOS/VLC"
alias lilypond="/Applications/LilyPond.app/Contents/Resources/bin/lilypond"

source ~/pdotfiles/bash/life_alarm.sh
