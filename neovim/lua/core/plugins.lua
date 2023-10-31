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
	'EdenEast/nightfox.nvim',
	'rcarriga/nvim-notify',
	'bling/vim-airline',
	'vim-airline/vim-airline-themes',
	'mhinz/vim-startify',

	-- LSP, tree-sitter
	{'nvim-treesitter/nvim-treesitter', build = ':TSUpdate'},
    'neovim/nvim-lspconfig',
    'hrsh7th/nvim-cmp',
    'hrsh7th/cmp-nvim-lsp',

	'nvim-lua/plenary.nvim',
	{
        'nvim-telescope/telescope.nvim',
        tag = '0.1.4',
        dependencies = {
            'nvim-lua/plenary.nvim'
        },
    },
	'tpope/vim-repeat',
	'tpope/vim-surround',
	'tpope/vim-commentary',
	'godlygeek/tabular',
	'jiangmiao/auto-pairs',

	-- 편집
	'airblade/vim-gitgutter',

	-- 언어
	'habamax/vim-asciidoctor'
})

