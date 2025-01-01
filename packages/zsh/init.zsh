#!/bin/zsh
# init config for some programs...

# cargo
[[ -d ~/.cargo ]] && export PATH=$PATH:~/.cargo/bin/

# asdf-vm
if [[ -d ~/.local/share/asdf ]]; then
  . ~/.local/share/asdf/asdf.sh
  . ~/.asdf/plugins/java/set-java-home.zsh
fi

# fzf
[[ -f ~/.fzf.zsh ]] && . ~/.fzf.zsh

[[ -d ~/.pack ]] && export PATH=$PATH:~/.pack/bin

# ghcup
[[ -d ~/.ghcup ]] && export PATH=$PATH:~/.ghcup/bin/

if (( $+commands[nnn] )); then
  if [[ ! -d ~/.config/nnn/plugins ]]; then
    echo "NNN plugins not installed"
    echo "install plugins with following commands:"
    echo 'curl -Ls https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs | sh'
  fi
  export NNN_PLUG='p:preview-tui'
  export NNN_FIFO=/tmp/nnn.fifo
fi
