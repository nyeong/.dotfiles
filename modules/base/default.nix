{
  palette,
  pkgs,
  lib,
  overlays,
  ...
}: {
  nixpkgs.overlays = overlays;
  imports = palette.lib.scanPaths ./.;

  nixpkgs.config.allowUnfree = true;
  nix = {
    package = pkgs.nix;
    optimise.automatic = true;
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      trusted-users =
        [
          "${palette.user.username}"
        ]
        ++ lib.optionals pkgs.stdenv.isDarwin ["@admin"]
        ++ lib.optionals pkgs.stdenv.isLinux ["root"];
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org"
      ];
      trusted-public-keys = ["cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="];
    };

    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };

  time.timeZone = "Asia/Seoul";
}
