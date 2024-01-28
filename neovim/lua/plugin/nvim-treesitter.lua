return function ()
  require('nvim-treesitter.configs').setup {
    ensure_installed = {
      "c",
      "cpp",
      "python",
      "lua",
      "vim",
      "javascript",
      "typescript",
      "html",
      "css",
      "vimdoc",
      "tsx",
      "rust",
      "ruby",
      "elixir",
      "heex",
      "haskell",
      "tsx",
    },
    sync_install = false,
    auto_install = true,
    highlight = {
      enable = true,
      additional_vim_regex_highlighting = false,
    },
    indent = { enable = true },
    autotag = { enable = true },
    rainbow = { enable = true },
    autopairs = { enable = true },
  }
end
