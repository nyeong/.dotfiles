# .dotfiles

My dotfiles for configuration.

## Dependencies

- package manager:
  - `brew` on macOS
  - `pacman` on ArchLinux
- git
- tmux
- zsh with [zplugin]
- neovim with [vim-plug]

[zplugin]: https://github.com/zdharma/zplugin
[vim-plug]: https://github.com/junegunn/vim-plug

## How to Use

Clone this repository on `$HOME/.dotfiles` and run `./.dotfiles/install.sh`.

```
git clone https://github.com/nyeong/.dotfiles
sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
ln -sf ~/.dotfiles/.zshrc ~
ln -sf ~/.dotfiles/.gitconfig ~
ln -sf ~/.dotfiles/.tmux.conf ~
mkdir -p ~/.config/nvim
ln -sf ~/.dotfiles/init.vim ~/.config/nvim
```

### VS Code

Extensions:

```
# backup
code --list-extensions > ~/.dotfiles/vs-extensions.txt

# restore
cat ~/.dotfiles/vs-extensions.txt | xargs -I % sh -c 'code --install-extension %'
```

Setting:

```
# linux
ln -sf ~/.dotfiles/vs-settings.json ~/.config/Code/User/settings.json
# macOS
ln -sf ~/.dotfiles/vs-settings.json ~/Library/Application\ Support/Code/User/settings.json
```
