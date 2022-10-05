#!/usr/bin/env bash

set -euxo pipefail

echo "Install stow..."
brew install stow
brew tap homebrew/cask-fonts
brew install font-hack-nerd-font

echo "Fetch all git submodules..."
git submodule update --recursive --init

rm -rf ~/.fzf
cp -r submodules/fzf ~/.fzf
~/.fzf/install --all

rm -rf ~/.vim
rm -f ~/.tmux.conf

echo "stow --target=$HOME vim tmux"
stow --target=$HOME vim nvim tmux
