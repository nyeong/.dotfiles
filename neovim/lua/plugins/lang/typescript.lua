return {
	"neovim/nvim-lspconfig",
	opts = {
		servers = {
			---@type lspconfig.options.tsserver
			tsserver = {
				on_init = function(client)
					client.server_capabilities.documentFormattingProvider = false
				end,
				settings = {
					completions = {
						completeFunctionCalls = true,
					},
				},
			},
		},
	},
}
