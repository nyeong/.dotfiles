{ config, pkgs, userConfig, ... }: {
  home.packages = with pkgs; [
    curl
    neovim
    fd
    ripgrep
    jq
    git
    lsd
    discord
    delta
  ];

  imports = [
    ./direnv
    ./wezterm
    ./zsh
    ./hammerspoon
    ./zellij
  ];
}
