local function map(mode, lhs, rhs, opts)
	local options = { noremap = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Space mode
map('n', '<Space>f', '<cmd>Telescope find_files<cr>')
map('n', '<Space>g', '<cmd>Telescope live_grep<cr>')
map('n', '<Space>b', '<cmd>Telescope buffers<cr>')
map('n', '<Space>h', '<cmd>Telescope help_tags<cr>')
map('n', '<Space>y', '"+y')
map('n', '<Space>Y', '"+Y')
map('n', '<Space>p', '"+p')
map('n', '<Space>P', '"+P')

-- goto mode
map('n', 'gh', '0')
map('n', 'gl', '$')
map('n', 'gn', ':bnext')
map('n', 'gp', ':bprev')

-- normal mode
map('', '<Esc><Esc>', ':let @/=""<cr>')
