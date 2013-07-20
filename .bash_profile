# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/login.defs
#umask 022

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# set PATH so it includes my private bin
export PATH="~/bin:${PATH}:/usr/games"

# Put MacPorts install dirs on the front of the path
if [ -d /opt/local/bin ]; then
  export PATH=/opt/local/bin:/opt/local/sbin:$PATH:./
  export MANPATH=/opt/local/share/man:$MANPATH
fi

# App Engine setup
if [ -d /opt/local/share/java/appengine-java-sdk ]; then
  export APPENGINE_HOME=/opt/local/share/java/appengine-java-sdk
  export PATH=$PATH:$APPENGINE_HOME/bin
fi

# Programmable completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
fi

if [ -f /opt/local/share/git-core/git-prompt.sh ]; then
    . /opt/local/share/git-core/git-prompt.sh
fi
