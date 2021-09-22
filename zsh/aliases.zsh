# don't make mistakes
alias mv='mv -i'
alias cp='cp -i'

if (( $+command[sk] )); then
  alias ck='cd ${sk}'
fi

# git
if (( $+command[git] )); then
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
  alias lt='exa -lTF --group-directories-first'
  alias tree='exa -TF --group-directories-first'
fi

# bat
if (( $+commands[bat] )); then
  export BAT_THEME='ansi'
  alias cat='bat --paging never --plain'
fi

# kakoune
if (( $+commands[kak] )); then
  alias k='kak'
fi

# WSL
if [[ -f /proc/sys/fs/binfmt_misc/WSLInterop ]]; then
  umask 022
  alias open=explorer.exe
  alias pbcopy=clip.exe
  alias pbpaste='powershell.exe Get-Clipboard | sed "s/\r$//" | head -c -1'
fi
