# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# include .profile if it hasn't run yet
if [ -z "$DID_PROFILE" ]; then
  . ~/.profile
fi

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# History controls
shopt -s histappend  # append to histfile instead of overwriting
# Undocumented feature to set history size to unlimited
export HISTSIZE=
export HISTFILESIZE=
export HISTTIMEFORMAT="[%F %T] "
export HISTFILE=~/.bash_eternal_history
# PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
# Don't put duplicate commands or commands staring with a space in the history
export HISTCONTROL=ignoreboth:erasedups

# VI mode on the command line
set -o vi

# Check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# Warn me before exiting if there are running jobs
shopt -s checkjobs

# Allow ** in globs
shopt -s globstar

# Make less more friendly for non-text input files, see lesspipe(1)
[ -x $HOME/bin/lesspipe.sh ] && eval "$($HOME/bin/lesspipe.sh)"

# Set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# Set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
*256color)
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '
    ;;
*)
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\W\$ '
    ;;
esac

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# Alias definitions
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Enable programmable completion features
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

if [ -d ~/src/ghar ]; then
  alias ghar=~/src/ghar/bin/ghar
  . ~/src/ghar/ghar-bash-completion.sh
fi

if [ -d ~/ghar ]; then
  alias ghar=~/ghar/bin/ghar
  . ~/ghar/ghar-bash-completion.sh
fi

if [ -f ~/bin/hub ]; then
  eval "$(hub alias -s)"
fi

# Shell colors
# BASE16_SHELL=$HOME/Documents/src/base16-shell
# [ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

# make vi my default, friendly editor
export EDITOR=vim

# Default flags to less
export LESS="-RSMqi"

# Function to call from ~/.local_config/bashrc to enforce a global tmux session
function __enable_global_tmux_session {
  if [ -z "$TMUX" ]; then
    session_id=`date +%Y%m%d%H%M%S`
    # Try to start a new session grouped with the global one, which will be
    # destroyed on detach. Otherwise, start the global session.
    # removed destroy-unattached because it doesn't work as will with full-time
    # WFH
      # set-option destroy-unattached \; \
    tmux -2 new-session -d -s $session_id -t "$USER" \; \
      attach-session -t $session_id ||
      tmux -2 new-session -s "$USER"
  fi
}

# Function to update a shell inside tmux with new environment variables
# (really useful for switching between ssh and local)
function update-environment {
  eval $(tmux show-environment ${1:+-t $1} -s)
  # local v
  # while read v; do
  #   if [[ $v == -* ]]; then
  #     unset ${v/#-/}
  #   else
  #     # Surround value with quotes
  #     v=${v/=/=\"}
  #     v=${v/%/\"}
  #     eval export $v
  #   fi
  # done < <(tmux show-environment)
}

# Local bashrc customization
if [ -f ~/.local_config/bashrc ]; then
    . ~/.local_config/bashrc
fi
