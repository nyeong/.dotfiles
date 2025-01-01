{ config, pkgs, ... }: {
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      local config = {}

      config.font = wezterm.font_with_fallback({
        'FiraCode Nerd Font Mono'
      })
      config.front_end = "WebGpu"
      config.color_scheme = 'nord'
      config.font_size = 14.0
      config.hide_tab_bar_if_only_one_tab = true
      config.window_padding = {
        left = 2,
        right = 2,
        top = 2,
        bottom = 2,
      }

      return config
    '';
  };
}
