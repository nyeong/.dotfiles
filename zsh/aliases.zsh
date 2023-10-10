# Declares aliases
# I use ...
# - git as a version control system
# - exa as a replacement of ls
# - bat as a replacement of cat
# - helix as a text editor

alias mv='mv -i'
alias cp='cp -i'

# git
if (( $+commands[git] )); then
  alias gs='git status'
  alias gc='git commit'
  alias gp='git push'
  alias gd='git diff'
  alias ga='git add'
  alias gl='git log --oneline --graph'
fi

# lsd
if (( $+commands[lsd] )); then
  alias ls='lsd'
  alias l='ls -l'
  alias ll='ls -la'
  alias la='ls -a'
  alias lt='ls --tree'
  alias tree='ls --tree'
fi

# bat
if (( $+commands[bat] )); then
  export BAT_THEME='ansi'
  alias cat='bat --paging never --plain'
  export PAGER=bat
fi

if (( $+commands[hx] )); then
  export EDITOR=hx
fi

if (( $+commands[helix] )); then
  export EDITOR=helix
  alias hx='helix'
fi

if (( $+commands[kak] )); then
  # export EDITOR=~/.dotfiles/kakoune/edit
fi
