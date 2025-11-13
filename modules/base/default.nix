{
  palette,
  pkgs,
  lib,
  overlays,
  config,
  ...
}: {
  nixpkgs.overlays = overlays;
  imports = palette.lib.scanPaths ./.;

  sops.secrets.github_fetch_token = {
    sopsFile = ../../secrets/personal.yaml;
    key = "github_fetch_token";
  };

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
    extraOptions = ''
      access-tokens = github.com=${config.sops.secrets.github_fetch_token.path}
    '';

    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };

  time.timeZone = "Asia/Seoul";
}
