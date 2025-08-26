{
  userConfig,
  pkgs,
  lib,
  ...
}: {
  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";
    matchBlocks = {
      # macOS에서 OrbStack을 쓸 때 연결하기 위함
      "orb" = lib.mkIf pkgs.stdenv.isDarwin {
        hostname = "localhost";
        port = 32222;
        identityFile = "~/.orbstack/ssh/id_ed25519";
      };

      "github.com" = {
        identitiesOnly = true;
        identityFile = [
          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin "/Users/${userConfig.username}/.ssh/id_ed25519")
        ];
      };
    };
  };
}
