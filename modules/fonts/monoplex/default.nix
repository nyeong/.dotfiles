{
  pkgs,
  userConfig,
  ...
}: let
  monoplexOverlay = import ./overlay.nix;
in {
  nixpkgs.overlays = [monoplexOverlay];

  fonts.packages = with pkgs; [monoplex-bin];
}
