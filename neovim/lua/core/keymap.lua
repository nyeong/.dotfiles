local function map(mode, lhs, rhs, opts)
	local options = { noremap = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- space mode
map('n', '<space>y', '"+y')
map('n', '<space>Y', '"+Y')
map('n', '<space>p', '"+p')
map('n', '<space>P', '"+P')

-- local opts = { buffer = ev.buf }

local telescope = require('telescope.builtin')
vim.keymap.set('n', '<space>f', telescope.find_files)
vim.keymap.set('n', '<space>g', telescope.live_grep)
vim.keymap.set('n', '<space>b', telescope.buffers)
vim.keymap.set('n', '<space>h', telescope.help_tags)
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)
vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
vim.keymap.set('n', '<space>wl', function()
	print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
end, opts)
vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
vim.keymap.set('n', '<space>k', vim.lsp.buf.hover, opts)
vim.keymap.set('n', '<space>K', vim.lsp.buf.signature_help, opts)
vim.keymap.set({ 'n', 'v' }, '<space>a', vim.lsp.buf.code_action, opts)

-- goto mode
vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
map('n', 'gh', '0')
map('n', 'gl', '$')
map('n', 'gs', '^')
map('n', 'gn', ':bnext<cr>')
map('n', 'gp', ':bprev<cr>')

-- normal mode
map('', '<Esc><Esc>', ':let @/=""<cr>')

-- next / prev mode
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
