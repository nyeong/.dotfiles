return {
	"nvim-telescope/telescope.nvim",
	dependencies = {
		{ "nvim-lua/plenary.nvim" },
		{ "nvim-telescope/telescope-fzf-native.nvim", enabled = vim.fn.executable("make") == 1, build = "make" },
	},
	cmd = "Telescope",
	keys = {
		-- find files
		{ "<space>ff", "<cmd>Telescope find_files<cr>", desc = "Find File" },
		{ "<space>fr", "<cmd>Telescope oldfiles<cr>", desc = "Open Recent File", noremap = false },
		{
			"<space>f/",
			function()
				require("telescope.builtin").live_grep()
			end,
			desc = "Open Recent File",
			noremap = false,
		},
		{ "<space>fb", "<cmd>Telescope buffers<cr>", desc = "Find buffers", noremap = false },
		{ "<space>sd", "<cmd>Telescope diagnostics bufnr=0<cr>", desc = "Document diagnostics" },
		{ "<space>sD", "<cmd>Telescope diagnostics<cr>", desc = "Workspace diagnostics" },
		{ "<space>sh", "<cmd>Telescope help_tags<cr>", desc = "Help Pages" },
		{ "<space>sH", "<cmd>Telescope highlights<cr>", desc = "Search Highlight Groups" },
		{ "<space>sk", "<cmd>Telescope keymaps<cr>", desc = "Key Maps" },
		{ "<space>sM", "<cmd>Telescope man_pages<cr>", desc = "Man Pages" },
		{ "<space>sm", "<cmd>Telescope marks<cr>", desc = "Jump to Mark" },
		{ "<space>so", "<cmd>Telescope vim_options<cr>", desc = "Options" },
		{ "<space>sR", "<cmd>Telescope resume<cr>", desc = "Resume" },
		-- git
		{ "<space>gc", "<cmd>Telescope git_commits<CR>", desc = "commits" },
		{ "<space>gs", "<cmd>Telescope git_status<CR>", desc = "status" },
	},
}
