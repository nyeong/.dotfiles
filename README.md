# .dotfiles

- **kakoune**을 텍스트 에디터로 사용합니다.
- **tmux**를 터미널 멀티플렉서로 사용합니다.
- **zsh**와 **p10k**로 쉘로 사용합니다.
- **git**을 버전 관리자로 사용합니다.

## 설치

```
git clone https://github.com/nyeong/.dotfiles ~

# kakoune
  mkdir -p ~/.config/kak
  git clone https://github.com/robertmeta/plug.kak.git ~/.config/kak/plugins/plug.kak
  ln -sf ~/.dotfiles/kak ~/.config/kak/kakrc

# tmux
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
  ln -sf ~/.dotfiles/tmux/tmux.conf ~/.tmux.conf

# zsh
  ln -sf ~/.dotfiles/zsh/zshrc       ~/.zshrc
  ln -sf ~/.dotfiles/zsh/aliases.zsh ~/.config/zsh/aliases.zsh
  ln -sf ~/.dotfiles/zsh/p10k.zsh    ~/.config/zsh/p10k.zsh
  cp     ~/.dotfiles/zsh/zshrc.local ~/.zshrc.local
  . ~/.zshrc 

# git
  ln -sf ~/.dotfiles/gitconfig ~/.gitconfig
```
