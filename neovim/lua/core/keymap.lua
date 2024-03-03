-- local telescope = require('telescope.builtin')
--
-- local config_path = '~/.dotfiles/neovim/lua/core/keymap.lua'
--
-- -- shortcut
-- vim.keymap.set('n', '%', ':%')
-- vim.keymap.set('n', 'm', '%') -- 괄호 짝 찾기
-- vim.keymap.set('v', 's', ':s/')
--
-- -- space mode
-- vim.keymap.set('n', '<space>?', ':e ' .. config_path .. '<cr>')
-- vim.keymap.set({ 'n', 'v' }, '<space>y', '"+y')
-- vim.keymap.set({ 'n', 'v' }, '<space>Y', '"+Y')
-- vim.keymap.set('n', '<space>p', '"+p')
-- vim.keymap.set('n', '<space>P', '"+P')
-- vim.keymap.set('n', '<space>E', 'ggVG')
-- vim.keymap.set('n', '<space>f', telescope.find_files)
-- vim.keymap.set('n', '<space>g', telescope.live_grep)
-- vim.keymap.set('n', '<space>b', telescope.buffers)
-- vim.keymap.set('n', '<space>h', telescope.help_tags)
-- vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
-- vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)
-- vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder)
-- vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder)
-- vim.keymap.set('n', '<space>wl', function()
--     print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
-- end)
-- vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition)
-- vim.keymap.set('n', '<space>k', vim.lsp.buf.hover)
-- vim.keymap.set('n', '<space>K', vim.lsp.buf.signature_help)
-- vim.keymap.set({ 'n', 'v' }, '<space>a', vim.lsp.buf.code_action)
--
-- -- language mode
-- vim.keymap.set('n', 'sr', vim.lsp.buf.rename)
--
-- -- goto mode
-- vim.keymap.set('n', 'gD', vim.lsp.buf.declaration)
-- vim.keymap.set('n', 'gd', vim.lsp.buf.definition)
-- vim.keymap.set('n', 'gi', vim.lsp.buf.implementation)
-- vim.keymap.set('n', 'gr', vim.lsp.buf.references)
-- vim.keymap.set({'n', 'v'}, 'gh', '0')
-- vim.keymap.set({'n', 'v'}, 'gl', '$')
-- vim.keymap.set({'n', 'v'}, 'gs', '^')
-- vim.keymap.set('n', 'gn', ':bnext<cr>')
-- vim.keymap.set('n', 'gp', ':bprev<cr>')
--
-- -- normal mode
vim.keymap.set("n", "<esc>", function()
	if vim.api.nvim_get_vvar("hlsearch") == 1 then
		vim.api.nvim_command("noh")
		vim.api.nvim_command("helpclose")
	end
	return "<esc>"
end, { noremap = true, silent = true })
--
-- -- next / prev mode
-- vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
-- vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
-- https://github.com/folke/which-key.nvim?tab=readme-ov-file#%EF%B8%8F-mappings

return {
	g = {
		name = "+goto",
		h = { "0", "Begin of the line", noremap = true },
		l = { "$", "End of the line" },
		s = { "^", "Begin of the line character" },
	},
}
