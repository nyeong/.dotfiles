return function ()
  require('nvim-treesitter.configs').setup({
    auto_install = true,
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
      "haskell",
    },
    hightlight = { enable = true },
    indent = { enable = true },
    autotag = { enable = true },
  })
end
