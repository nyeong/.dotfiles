{ config, pkgs, ... }:
{
  services.emacs.package = pkgs.emacs-unstable;

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };
  };
}
