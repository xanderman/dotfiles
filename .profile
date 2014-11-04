# ~/.profile: executed by X for graphical sessions (bash uses .bash_profile)

# Local profile customization
if [ -f ~/.local_config/profile ]; then
    . ~/.local_config/profile
fi

# export all system-wide variables I want inherited by applications

# set PATH so it includes my private bin
export PATH="$HOME/bin:${PATH}"

# Tell Chrome to use the proxy (system settings don't work in xmonad)
# export auto_proxy="https://proxyconfig.corp.google.com/wpad.dat"

# Get my timezone right
export TZ="America/Denver"

export DID_PROFILE=1
