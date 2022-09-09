#!/bin/zsh
# init config for some programs...

# cargo
[[ -d ~/.cargo ]] && export PATH=$PATH:~/.cargo/bin/

# asdf-vm
[[ -d ~/.asdf ]] && . ~/.asdf/asdf.sh

# fzf
[[ -f ~/.fzf.zsh ]] && . ~/.fzf.zsh
