if [[ -d ~/.zinit ]]; then
  source ~/.zinit/bin/zinit.zsh
  autoload -Uz _zinit
  (( ${+_comps} )) && _comps[zinit]=_zinit

  ZSH_EXPAND_ALL_DISABLE=word

  # 여기에 플러그인을 추가
  zinit light simnalamburt/zsh-expand-all
  zinit light zdharma/fast-syntax-highlighting
  zinit light zsh-users/zsh-completions
  zinit light zsh-users/zsh-autosuggestions
  zinit light zsh-users/zsh-history-substring-search
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down

  zinit ice pick"async.zsh" src"pure.zsh"
  zinit light sindresorhus/pure

  autoload -Uz compinit
  compinit

  zinit cdreplay
fi

zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu select
export TIME_STYLE='long-iso'

alias mv='mv -i'
alias cp='cp -i'

if (( $+commands[git] )); then
  alias gs='git status'
  alias gc='git commit'
  alias gp='git push'
  alias gd='git diff'
  alias ga='git add'
fi

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
elif (( $+commands[vim] )); then
  export EDITOR=vim
  alias v='vim'
  alias vi='vim'
  alias vim='vim'
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
  export BAT_THEME="ansi-dark"
  alias cat='bat --paging never --plain'
fi

if [[ -f ~/.zshrc.local ]]; then
  source ~/.zshrc.local
fi
