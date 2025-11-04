{
  palette,
  pkgs,
  lib,
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
}
