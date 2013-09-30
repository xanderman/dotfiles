# My commonly used aliases

alias rmorig='find . -name "*.orig" -delete'
alias clean='rm -f *~ *#'
alias ptree='ps -aef --forest'
alias cronls='sudo ls /var/spool/cron/crontabs'

# Set some default options for various commands
alias diff=colordiff
alias screen='TERM=screen screen'
alias vi='vim -p'

# enable color support of ls
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b ~/.dircolors`"
    alias ls='ls --color=auto'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias lal='ls -A -l'

# More navigation aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Open a new firefox process
alias ff='env MOZ_NO_REMOTE=1 firefox -ProfileManager &'

# Decimal/Hex converter functions
function h2d {
  echo "obase=10; $@" | bc
}

function d2h {
  echo "obase=16; $@" | bc
}

# Local alias definitions
if [ -f ~/.local_config/bash_aliases ]; then
    . ~/.local_config/bash_aliases
fi

# vim: filetype=sh
