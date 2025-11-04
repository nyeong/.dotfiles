{
  pkgs,
  lib,
  ...
}: {
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      local wezterm = require("wezterm")
      local config = {}
      config.color_scheme = "Nord (Gogh)"
      config.font = wezterm.font_with_fallback({
        "Liga SFMono Nerd Font",
        "Monoplex KR Wide Nerd",
      })
      config.font_size = 14.0
      config.window_padding = {
        left = 10,
        right = 10,
        top = 10,
        bottom = 10,
      }
      return config
    '';
  };

  # OrbStack defaults are set via nix-darwin in host config
}
