# .dotfiles

My dotfiles for configuration.

## Dependencies

- [zplugin](https://github.com/zdharma/zplugin) as a zsh plugin manager.
- [vim-plug](https://github.com/junegunn/vim-plug) as a neovim plugin manager.
- [tpm](https://github.com/tmux-plugins/tpm) as a tmux plugin manager.

## Installation

```bash
cd ~
git clone https://github.com/nyeong/.dotfiles
ln -sf ~/.dotfiles/zshrc     ~/.zshrc
ln -sf ~/.dotfiles/gitconfig ~/.gitconfig
ln -sf ~/.dotfiles/tmux.conf ~/.tmux.conf
mkdir -p ~/.config/nvim
ln -sf ~/init.vim            ~/.config/nvim/init.vim

# seoul256-iterm
git clone https://github.com/junegunn/seoul256.vim
open seoul256.vim/iterm2/*
rm -rf seoul256.vim
```

I use ...

- **tmux** as a terminal multiplexer.
- **neovim** as a text editor.
- **iTerm2** as a terminal simulator on macOS.
- **seoul256** as a color scheme for everywhere.

## References

- [simnalamburt/.dotfiles](https://github.com/simnalamburt/.dotfiles)
- [junegunn/dotfiles](https://github.com/junegunn/dotfiles)
- [mdo/config](https://github.com/mdo/config)
