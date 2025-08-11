{ userConfig, pkgs, lib, ... }: {
  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";
    matchBlocks = {
      "github.com" = {
        identitiesOnly = true;
        identityFile = [
          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin "/Users/${userConfig.username}/.ssh/id_ed25519")
        ];
      };
    };
  };
}