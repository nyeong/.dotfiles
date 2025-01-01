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
  ];

  imports = [
    ./direnv
    ./wezterm
    ./zsh
  ];
}
