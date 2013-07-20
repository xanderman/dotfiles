# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# include .profile if it hasn't run yet
if [ -z "$DID_PROFILE" ]; then
  . ~/.profile
fi

# Global tmux session on my phsyical machine
if [[ "$TERM" != "screen" ]]
then
  session_id=`date +%Y%m%d%H%M%S`
  # Try to start a new session grouped with the global one, which will be
  # destroyed on detach. Otherwise, start the global session.
  tmux new-session -d -s $session_id -t "$USER" \; \
    set-option destroy-unattached \; \
    new-window -t $session_id \; \
    attach-session -t $session_id ||
    tmux -2 new-session -s "$USER"
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

# Make less more friendly for non-text input files, see lesspipe(1)
[ -x /opt/local/bin/lesspipe.sh ] && eval "$(lesspipe.sh)"

# Set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

# Set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
xterm-color|screen*)
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

# Give ls a colorful output
export CLICOLOR=1
export LSCOLORS=gxfxcxdxcxegedabaggxgx

# Get the name of the git branch if present
function __make_repo_prompt_part {
  local p="$(__git_ps1 '%s')"
  if [ -z "$p" ]; then
    echo ""
  else
    # Git client, return repo:branch
    echo "($p) "
  fi
}

export PS1="\$(__make_repo_prompt_part)$PS1"
