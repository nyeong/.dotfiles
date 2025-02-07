{ pkgs, user }:
let
  monoplex = import ./packages.nix;
in
{
  nixpkgs.overlays = [
    monoplex
  ];

  fonts.packages = with pkgs; [
    monoplex-bin
  ];
}