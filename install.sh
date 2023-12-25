#!/usr/bin/env bash

set -euxo pipefail

export PLATFORM=$(uname -s)

if [ "$PLATFORM" = Linux ]; then
   sudo apt-get update
   sudo apt-get install -y stow

   mkdir --parent ~/.fonts

   if [ ! -f ~/.fonts/Go-Mono.tar.xz ]; then
    curl -L -o ~/.fonts/Go-Mono.tar.xz \
        https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/Go-Mono.tar.xz
    tar xvf ~/.fonts/Go-Mono.tar.xz -C ~/.fonts/
    sudo fc-cache -fv
   fi
elif [ "$PLATFORM" = Darwin ]; then
    brew install stow
    brew tap homebrew/cask-fonts
    brew install font-hack-nerd-font
fi

echo "Fetch all git submodules..."
git submodule update --recursive --init

rm -rf ~/.fzf
cp -r submodules/fzf ~/.fzf
~/.fzf/install --all

rm -rf ~/.vim
rm -f ~/.tmux.conf

echo "stow --target=$HOME vim tmux"
stow --target=$HOME vim nvim tmux
