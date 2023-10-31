local wezterm = require 'wezterm'
local config = {}

config.color_scheme = 'nord'
config.font = wezterm.font({
    family = 'Monaspace Neon',
    harfbuzz_features = { 'ss01', 'ss02', 'ss03', 'ss04', 'ss05', 'ss06', 'ss07', 'ss08', 'calt', 'dlig' },
})
config.font_size = 13

return config
