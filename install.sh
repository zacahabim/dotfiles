!/usr/bin/env bash

 set -euxo pipefail

 export PLATFORM=$(uname -s)

 if [ "$PLATFORM" = Linux ]; then
    # sudo apt-get update
    # sudo apt-get install -y stow golang python3-pip python3.10-venv ranger
    # curl -fsSL https://deb.nodesource.com/setup_21.x | sudo -E bash - &&\
    #     sudo apt-get install -y nodejs

    mkdir --parent ~/.fonts
    for font in Go-Mono.tar.xz ComicShannsMono.tar.xz MPlus.tar.xz ; do
        if [ ! -f ~/.fonts/"${font}" ]; then
            curl -L -o ~/.fonts/"${font}" \
                https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/"${font}"
            tar xvf ~/.fonts/"${font}" -C ~/.fonts/
            sudo fc-cache -fv
        fi
    done
 elif [ "$PLATFORM" = Darwin ]; then
     brew install stow golang node python3
     pip3 install virtualenv
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
stow --target=$HOME vim nvim tmux ranger
