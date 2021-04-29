#!/usr/bin/env bash

set -euo pipefail

rm -rf ~/.fzf

cp -r submodules/fzf ~/.fzf
~/.fzf/install --all

stow vim tmux i3
