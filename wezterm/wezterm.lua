local wezterm = require 'wezterm'
local config = {}

local harfbuzz_features = {
    'ss01', 'ss02', 'ss03', 'ss04', 'ss05', 'ss06', 'ss07', 'ss08', 'calt', 'dlig',
}

config.color_scheme = 'nord'
config.font = wezterm.font({
    family = 'MonaspiceNe NFM',
    harfbuzz_features = harfbuzz_features,
})
config.font_rules = {
    {
        italic = false,
        intensity = 'Normal',
        font = wezterm.font{
            family = 'MonaspiceNe NFM',
            weight = 'Regular',
            harfbuzz_features = harfbuzz_features,
        },
    },
    {
        intensity = 'Bold',
        font = wezterm.font{
            family = 'MonaspiceKr NFM',
            weight = 'Black',
            harfbuzz_features = harfbuzz_features,
        },
    },
    {
        italic = true,
        font = wezterm.font{
            family = 'MonaspiceRn NFM',
            weight = 'Regular',
            harfbuzz_features = harfbuzz_features,
        },
    },
}
config.font_size = 13

return config
