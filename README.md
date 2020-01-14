# .dotfiles

## Dependencies

- git
- zsh
  - [zplugin](https://github.com/zdharma/zplugin)
- neovim
  - [vim-plug](https://github.com/junegunn/vim-plug)

## How to Use

```
# clone
git clone https://github.com/nyeong/.dotfiles ~/.dotfiles

# zshrc
ln -sf ~/.dotfiles/.zshrc ~

# git
ln -sf ~/.dotfiles/.gitconfig ~

# tmux
ln -sf ~/.dotfiles/.tmux.config ~

# neovim
mkdir -p ~/.config/nvim
ln -sf ~/.dotfiles/init.vim ~/.config/nvim
```

### VS Code

Extensions:

```
# when backup
code --list-extensions > ~/.dotfiles/vs-extensions.txt

# when restore
cat ~/.dotfiles/vs-extensions.txt | xargs -I % sh -c 'code --install-extension %'
```

Setting:

```
# linux
ln -sf ~/.dotfiles/vs-settings.json ~/.config/Code/User/settings.json
# macOS
ln -sf ~/.dotfiles/vs-settings.json ~/Library/Application\ Support/Code/User/settings.json
# windows
잘 모르겠어욤 ㅎㅎ;
```
