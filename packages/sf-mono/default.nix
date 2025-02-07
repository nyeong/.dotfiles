{ pkgs, user }:
let
  sf-mono = import ./packages.nix;
in
{
  nixpkgs.overlays = [
    sf-mono
  ];

  fonts.packages = with pkgs; [
    sf-mono-liga-bin
  ];
}