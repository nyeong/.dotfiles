if [[ -d ~/.zplugin ]]; then
  source ~/.zplugin/bin/zplugin.zsh
  autoload -Uz _zplugin
  (( ${+_comps} )) && _comps[zplugin]=_zplugin

  ZSH_EXPAND_ALL_DISABLE=word

  # 여기에 플러그인을 추가
  zplugin light simnalamburt/zsh-expand-all
  zplugin light zdharma/fast-syntax-highlighting
  zplugin light zsh-users/zsh-completions
  zplugin light zsh-users/zsh-autosuggestions
  zplugin light zsh-users/zsh-history-substring-search
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down

  zplugin ice pick"async.zsh" src"pure.zsh"
  zplugin light sindresorhus/pure

  autoload -Uz compinit
  compinit

  zplugin cdreplay
fi

zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu select
export TIME_STYLE='long-iso'

alias mv='mv -i'
alias cp='cp -i'

# exa
if (( $+commands[exa] )); then
  alias l='exa -alhF --group-directories-first'
  alias ls='exa -F --group-directories-first'
  alias ll='exa -lhF --group-directories-first'
  alias la='exa -alhgF --group-directories-first'
  alias lt='exa -lTF --group-directories-first --git-ignore'
  alias tree='exa -TF --group-directories-first --git-ignore'
fi

# neovim
if (( $+commands[nvim] )); then
  export EDITOR=nvim
  alias v='nvim'
  alias vi='nvim'
  alias vim='nvim'
fi

if [[ -f ~/.fzf.zsh ]]; then
  source ~/.fzf.zsh
fi

# rust
if [[ -d ~/.cargo ]]; then
  source ~/.cargo/env
fi

# rbenv
if (( $+commands[rbenv] )); then
  eval "$(rbenv init -)"
fi

# pyenv
if (( $+commands[pyenv] )); then
  eval "$(pyenv init -)"
fi

# go
if (( $+commands[go] )); then
  export GOPATH=~/.go
fi

# fasd
if (( $+commands[fasd] )); then
  eval "$(fasd --init auto)"
fi

# bat
if (( $+commands[bat] )); then
  export BAT_THEME="OneHalfDark"
  alias cat='bat --paging never --plain'
fi
