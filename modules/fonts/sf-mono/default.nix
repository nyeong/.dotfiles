{ pkgs, userConfig, ... }:
let sfMonoOverlay = import ./overlay.nix;
in {
  nixpkgs.overlays = [ sfMonoOverlay ];

  fonts.packages = with pkgs; [ sf-mono-liga-bin ];
}
