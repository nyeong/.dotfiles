return function ()
local lspconfig = require('lspconfig')

-- neovim
require("neodev").setup({})

lspconfig.tsserver.setup{
    javascript = {
        inlayHints = {
            includeInlayEnumMemberValueHints = true,
            includeInlayFunctionLikeReturnTypeHints = true,
            includeInlayFunctionParameterTypeHints = true,
            includeInlayParameterNameHints = 'all',
            includeInlayParameterNameHintsWhenArgumentMatchesName = true,
            includeInlayPropertyDeclarationTypeHints = true,
            includeInlayVariableTypeHints = true,
        }
    },
    typescript = {
            includeInlayEnumMemberValueHints = true,
            includeInlayFunctionLikeReturnTypeHints = true,
            includeInlayFunctionParameterTypeHints = true,
            includeInlayParameterNameHints = 'all',
            includeInlayParameterNameHintsWhenArgumentMatchesName = true,
            includeInlayPropertyDeclarationTypeHints = true,
            includeInlayVariableTypeHints = true,
    }
}
lspconfig.lua_ls.setup{
on_init = function(client)
local path = client.workspace_folders[1].name
if not vim.loop.fs_stat(path..'/.luarc.json') and not vim.loop.fs_stat(path..'/.luarc.jsonc') then
  client.config.settings = vim.tbl_deep_extend('force', client.config.settings, {
    Lua = {
        diagnostics = {
        globals = {'vim'},
            },
          runtime = {
            -- Tell the language server which version of Lua you're using
            -- (most likely LuaJIT in the case of Neovim)
            version = 'LuaJIT'
          },
          -- Make the server aware of Neovim runtime files
          workspace = {
            checkThirdParty = false,
            library = {
              vim.env.VIMRUNTIME
              -- "${3rd}/luv/library"
              -- "${3rd}/busted/library",
            }
            -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
            -- library = vim.api.nvim_get_runtime_file("", true)
          }
        }
      })

      client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
    end
    return true
  end

}
lspconfig.ruby_ls.setup{}
lspconfig.elixirls.setup{
    cmd = { 'elixir-ls' };
    filetypes = { 'ex' }
}

-- inlay hints
if vim.lsp.inlay_hint then
    vim.keymap.set('n', '<space>i', function ()
        vim.lsp.inlay_hint(0, nil)
    end, { desc = 'toggle inlay hints' })
end


-- nvim-cmp for auto complete
local capabilities = require("cmp_nvim_lsp").default_capabilities()

local servers = {
    'tsserver',
    'lua_ls',
    'elixirls',
    'ruby_ls',
}

local root_dir = function()
    return vim.fn.getcwd()
end

for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup {
        root_dir = root_dir,
        capabilities = capabilities
    }
end

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
        -- Enable completion triggered by <c-x><c-o>

        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
    end,
})
end
