return {
	{
		"echasnovski/mini.pairs",
		event = "VeryLazy",
		version = "*",
		init = function()
			require("mini.pairs").setup()
		end,
	},
	{
		"echasnovski/mini.surround",
		version = "*",
		init = function()
			require("mini.surround").setup()
		end,
	},
	{
		"echasnovski/mini.comment",
		event = "VeryLazy",
		version = "*",
		init = function()
			require("mini.comment").setup()
		end,
	},
}
