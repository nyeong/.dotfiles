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

  sops.templates."nix-access-tokens".content = ''
    access-tokens = github.com:$(cat ${config.sops.secrets.github_fetch_token.path})
  '';

  programs.ssh.startAgent = true;

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
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      builders-use-substitutes = true;
    };
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
  };

  environment.etc."nix/nix.conf.d/access-tokens.conf".source = config.sops.templates."nix-access-tokens".path;

  time.timeZone = "Asia/Seoul";
}
