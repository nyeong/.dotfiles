{pkgs, ...}: let
  cursor-cli = pkgs.callPackage ./overlay/cursor-cli.nix {};
in {
  home.packages = [
    cursor-cli
  ];
}
