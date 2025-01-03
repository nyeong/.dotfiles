ZINIT_HOME=~/.local/share/zinit/zinit.git
if [[ ! -d $ZINIT_HOME ]]; then
  echo "zinit can not be loaded"
fi

source $ZINIT_HOME/zinit.zsh

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# p10k
zinit ice depth=1;
zinit light romkatv/powerlevel10k

# auto suggestion
ZSH_AUTOSUGGEST_USE_ASYNC=1
zinit light zsh-users/zsh-autosuggestions

zinit light agkozak/zsh-z

# expand aliases
ZSH_EXPAND_ALL_DISABLE=word
zinit light simnalamburt/zsh-expand-all
zinit light simnalamburt/ctrlf
zinit light zdharma/fast-syntax-highlighting
zinit light zsh-users/zsh-history-substring-search
# bind up-arrow and down-arrow
bindkey -e
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# zsh substring completion
setopt complete_in_word
setopt always_to_end
WORDCHARS=''
zmodload -i zsh/complist

# tap completion
zinit light zsh-users/zsh-completions
autoload -Uz compinit bashcompinit
compinit
bashcompinit
zinit cdreplay


# make menu looks lincer
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

export TIME_STYLE='long-iso'
export GPG_TTY=$(tty)
