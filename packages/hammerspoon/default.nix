{ config, pkgs, lib, ... }:
let
  hammerspoon = pkgs.callPackage ./package.nix {};
in {
  home.packages = [ hammerspoon ];
  home.file.".config/hammerspoon" = {
    source = ./config;
    recursive = true;
  };
}
