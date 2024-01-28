-- install lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
	"git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

-- load plugins
require('lazy').setup({
    -- Outlooks
    'projekt0n/github-nvim-theme',
    'EdenEast/nightfox.nvim',
    'rcarriga/nvim-notify',
    {'nvim-lualine/lualine.nvim', config = require('plugin.lualine')},
    'nvim-tree/nvim-tree.lua',
    'nvim-tree/nvim-web-devicons',
    'mhinz/vim-startify',
    {
        'akinsho/bufferline.nvim',
        version = "*",
        dependencies = 'nvim-tree/nvim-web-devicons',
        config = function () require('bufferline').setup {} end,
    },

    -- text
    'tpope/vim-sleuth', -- automatically adjusts shiftwidth, expandtab

    -- LSP, tree-sitter
    {
        'nvim-treesitter/nvim-treesitter',
        dependencies = { 'HiPhish/nvim-ts-rainbow2' },
        config = require 'plugin.nvim-treesitter',
        build = ':TSUpdate',
    },
    {
        'neovim/nvim-lspconfig',
        opts = { inlay_hints = { enabled = true } },
        config = require 'plugin.lspconfig'
    },
    { 'hrsh7th/nvim-cmp', config = require('plugin.nvim-cmp') },
    'hrsh7th/cmp-nvim-lsp',
    -- ctags
    {'ludovicchabant/vim-gutentags', config = require 'plugin.vim-gutentags' },
    {'majutsushi/tagbar', config = require 'plugin.tagbar'},

    'nvim-lua/plenary.nvim',
    {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.4',
        dependencies = { 'nvim-lua/plenary.nvim' },
    },
    'tpope/vim-repeat',
    'tpope/vim-surround',
    'tpope/vim-commentary',
    'godlygeek/tabular',
    'jiangmiao/auto-pairs',

    -- 편집
    'airblade/vim-gitgutter',

    -- 언어
    { 'habamax/vim-asciidoctor', config = require 'plugin.asciidoctor' },
    { "folke/neodev.nvim", opts = {} }
})

