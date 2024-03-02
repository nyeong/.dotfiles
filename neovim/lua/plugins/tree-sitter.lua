return {
	{
		"nvim-treesitter/nvim-treesitter",
		dependencies = { "HiPhish/nvim-ts-rainbow2" },
		config = require("plugin.nvim-treesitter"),
		build = ":TSUpdate",
	},
}
