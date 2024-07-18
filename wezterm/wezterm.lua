local wezterm = require("wezterm")
local config = {}

local harfbuzz_features = {
	"ss01",
	"ss02",
	"ss03",
	"ss04",
	"ss05",
	"ss06",
	"ss07",
	"ss08",
	"calt",
	"dlig",
}

config.color_scheme = "nord"
config.line_height = 1.1
config.font = wezterm.font_with_fallback({
	"Monoplex KR Wide Nerd",
	"Goorm Sans",
})
config.font_size = 14

return config
