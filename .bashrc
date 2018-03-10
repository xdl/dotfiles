# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

export PATH=/usr/local/bin:/usr/local/sbin:~/bin:$PATH

# Caveat for brew install python
#http://docs.python-guide.org/en/latest/starting/install/osx/
export PATH="/usr/local/opt/python@2/libexec/bin:$PATH"


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
