{
  palette,
  pkgs,
  lib,
  config,
  ...
}: {
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "*" = {
        addKeysToAgent = "yes";
      };
      # macOS에서 OrbStack을 쓸 때 연결하기 위함
      "orb" = lib.mkIf pkgs.stdenv.isDarwin {
        hostname = "localhost";
        port = 32222;
        identityFile = "~/.orbstack/ssh/id_ed25519";
      };

      "github.com" = {
        identitiesOnly = true;
        identityFile =
          if pkgs.stdenv.isDarwin
          then "/Users/${palette.user.username}/.ssh/id_ed25519"
          else "/home/${palette.user.username}/.ssh/id_ed25519";
      };
    };
  };

  sops.secrets = {
    "ssh/id_ed25519" = {
      sopsFile = ../../secrets/personal.yaml;
      key = "ssh_private_key";
      path = "${config.home.homeDirectory}/.ssh/id_ed25519";
      mode = "0600";
    };
    "ssh/id_ed25519.pub" = {
      sopsFile = ../../secrets/personal.yaml;
      key = "ssh_public_key";
      path = "${config.home.homeDirectory}/.ssh/id_ed25519.pub";
      mode = "0644";
    };
  };
}
