local wezterm = require 'wezterm'
local config = {}

config.color_scheme = 'nord'
config.font = wezterm.font('Fira Code', {
  dpi = 114.0,
  font_size = 14.0
})

return config

