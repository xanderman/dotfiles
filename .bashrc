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
export HISTSIZE=100000
export HISTFILESIZE=10000000
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

if [ -d ~/ghar ]; then
  alias ghar=~/ghar/bin/ghar
  . ~/ghar/ghar-bash-completion.sh
fi

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
    tmux -2 new-session -d -s $session_id -t "$USER" \; \
      set-option destroy-unattached \; \
      attach-session -t $session_id ||
      tmux -2 new-session -s "$USER"
  fi
}

# Local bashrc customization
if [ -f ~/.local_config/bashrc ]; then
    . ~/.local_config/bashrc
fi
