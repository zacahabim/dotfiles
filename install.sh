#!/usr/bin/env bash

set -euo pipefail

rm -rf ~/.fzf

cp -r submodules/fzf ~/.fzf
~/.fzf/install --all

rm -rf ~/.vim
rm ~/.tmux.conf
stow --target=$HOME vim tmux xsession
