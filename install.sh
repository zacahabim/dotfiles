#!/usr/bin/env bash

set -euxo pipefail

echo "Install stow..."
sudo apt-get update
sudo apt-get install stow

echo "Fetch all git submodules..."
git submodule update --recursive --init

rm -rf ~/.fzf
cp -r submodules/fzf ~/.fzf
~/.fzf/install --all

rm -rf ~/.vim
rm -f ~/.tmux.conf

echo "stow --target=$HOME vim tmux"
stow --target=$HOME vim nvim tmux
