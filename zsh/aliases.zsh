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

if (( $+commands[nvim] )); then
  alias vi='nvim'
  alias vim='nvim'
  export EDITOR='nvim'

  # EDITOR를 vim 계열로 설정하면 bindkey가 풀립니다...!
  bindkey -e
fi
