export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$XDG_CONFIG_HOME/local/share"
export XDG_CACHE_HOME="$XDG_CONFIG_HOME/cache"

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export HISTFILE="$ZDOTDIR/.zhistory"    # History filepath
export HISTSIZE=10000                   # Maximum events for internal history
export SAVEHIST=10000                   # Maximum events in history file

PATH="$HOME/.local/bin:$PATH"

export NINESTUFFS="$HOME/.local/9stuffs"
PATH="$NINESTUFFS/bin:$PATH"

export PLAN9="$HOME/.local/plan9port"
PATH="$PLAN9/bin:$PATH"
export PATH
