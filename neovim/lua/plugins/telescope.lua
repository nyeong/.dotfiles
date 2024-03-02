return {
	"nvim-telescope/telescope.nvim",
	dependencies = {
		{ "nvim-lua/plenary.nvim" },
		{ "nvim-telescope/telescope-fzf-native.nvim", enabled = vim.fn.executable("make") == 1, build = "make" },
	},
	cmd = "Telescope",
	keys = {
		{ "<space>f", "<cmd>Telescope find_files<cr>", desc = "Find File" },
		{ "<space>r", "<cmd>Telescope oldfiles<cr>", desc = "Open Recent File", noremap = false },
	},
}
