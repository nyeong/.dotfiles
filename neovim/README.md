# neovim config

## Directory Structure

```
 neovim
├──  init.lua
├──  README.md
├──  ftplugin -- Config per language
│   ├──  asciidoc.lua
│   └──  typescript.lua
└──  lua
    ├──  core
    │   ├──  autocmd.lua
    │   ├──  config.lua -- default vim configs
    │   ├──  keymap.lua
    │   └──  lazy.lua -- lazy.vim
    └──  plugins -- plugin config per purpose
        ├──  lspconfig.lua
        ├──  outlooks.lua
        └──  tree-sitter.lua
```

## Refer

- https://github.com/neoclide/coc.nvim/
- https://github.com/m4xshen/dotfiles/tree/main/nvim/nvim
- https://www.lazyvim.org/keymaps#neo-treenvim
- https://github.com/AstroNvim/AstroNvim/tree/main
