{ config, lib, pkgs, ... }:
let
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
    settings = {
      # Register known devices (only those with non-empty id)
      devices = devices;

      folders = {
        dotfiles = {
          id = "dotfiles";
          path = "~/.dotfiles";
          devices = deviceNames;
          # Exclude the git repo internals from sync
          ignorePatterns = [ ".git" ];
        };
      };
    };
  };
}


