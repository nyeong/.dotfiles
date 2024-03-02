return {
    -- "EdenEast/nightfox.nvim",
    {
        'shaunsingh/nord.nvim',
        config = function()
            -- load the colorscheme here
            vim.cmd([[colorscheme nord]])
        end,

    }
    -- "projekt0n/github-nvim-theme",
    -- "rcarriga/nvim-notify",
    -- { "nvim-lualine/lualine.nvim", config = require("plugin.lualine") },
    -- "nvim-tree/nvim-tree.lua",
    -- "nvim-tree/nvim-web-devicons",
}
