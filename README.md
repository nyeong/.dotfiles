# .dotfiles

- [helix](https://helix-editor.com/)를 텍스트 에디터로 사용합니다.
- [tmux](https://github.com/tmux/tmux)를 터미널 멀티플렉서로 사용합니다.
- [zsh](https://www.zsh.org/)와 [p10k](https://github.com/romkatv/powerlevel10k)로 쉘로 사용합니다.
- [git](https://git-scm.com/)을 버전 관리자로 사용합니다.

## 설치

```
git clone https://github.com/nyeong/.dotfiles ~

# tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -sf ~/.dotfiles/tmux/tmux.conf ~/.tmux.conf

# zsh
mkdir -p ~/.config/zsh
ln -sf ~/.dotfiles/zsh/zshrc ~/.zshrc
. ~/.zshrc 

# git
ln -sf ~/.dotfiles/git/gitconfig ~/.gitconfig
```
