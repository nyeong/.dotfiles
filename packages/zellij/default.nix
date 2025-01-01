{ config, home, pkgs, ... }: {

  home.file.".config/zellij" = {
    source = ./config;
    recursive = true;
  };

  programs.zellij = {
    enable = true;
  };
}
