#!/usr/bin/env bash

rm -rf ~/.fzf

cp -r submodules/fzf ~/.fzf
~/.fzf/install --all

stow vim tmux
