{
  config,
  lib,
  pkgs,
  ...
}: let
  devices = {
    "nyeong-air" = {
      id = "A5GD47O-W3OI5WT-QJ4XHFT-BXKN3UK-BU6XIQ2-5MCVG46-D3DLYQS-TLKPZA2";
    };
    "nixbox" = {
      id = "CUSYAW3-XKCSZ3N-3TMMBTQ-OGI37SE-DIOUHUS-KGFS2SX-WWKL2IQ-I3OLTQ2";
    };
  };
  deviceNames = builtins.attrNames devices;
in {
  services.syncthing = {
    enable = true;
    guiAddress = "localhost:8384";
    settings = {
      # Register known devices (only those with non-empty id)
      devices = devices;

      folders = {
        "screenshots@nyeong-air" = {
          id = "screenshots@nyeong-air";
          devices = ["nyeong-air" "nixbox"];
          # path는 각 호스트의 home-manager에서 정의
          ignores = [
            ".git"
            ".DS_Store"
            "**/.git/**"
          ];
        };

        hanassig = {
          id = "hanassig";
          # path는 각 호스트의 home-manager에서 정의
          devices = deviceNames;
          ignores = [
            ".git"
            ".DS_Store"
            "**/.git/**"
          ];
        };

        dotfiles = {
          id = "dotfiles";
          # path는 각 호스트의 home-manager에서 정의
          devices = deviceNames;
          ignores = [
            ".git"
            ".DS_Store"
            "**/.git/**"
          ];
        };
      };
    };
  };
}
