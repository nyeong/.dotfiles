local wezterm = require 'wezterm'
local config = {}

local harfbuzz_features = {
    'ss01', 'ss02', 'ss03', 'ss04', 'ss05', 'ss06', 'ss07', 'ss08', 'calt', 'dlig',
}
local NeoDunggeunmoFont = {
    family = 'NeoDunggeunmo Code',
    scale = 1.2
}

config.color_scheme = 'nord'
config.line_height = 1.13
config.font = wezterm.font_with_fallback {
    NeoDunggeunmoFont,
    {
        family = 'MonaspiceNe NFM',
        harfbuzz_features = harfbuzz_features,
    },
}
config.font_rules = {
    {
        intensity = 'Bold',
        font = wezterm.font_with_fallback {
            { family = 'MonaspiceKr NFM', weight = 'Black', harfbuzz_features = harfbuzz_features },
            NeoDunggeunmoFont,
        },
    },
    {
        italic = true,
        font = wezterm.font_with_fallback {
            { family = 'MonaspiceRn NFM', weight = 'Regular', harfbuzz_features = harfbuzz_features, },
            NeoDunggeunmoFont,
        },
    },
}
config.font_size = 13

return config
