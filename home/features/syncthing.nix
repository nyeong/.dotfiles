{
  lib,
  config,
  ...
}: let
  cfg = config.features.syncthing;
  devices = {
    "nyeong-air" = {
      id = "A5GD47O-W3OI5WT-QJ4XHFT-BXKN3UK-BU6XIQ2-5MCVG46-D3DLYQS-TLKPZA2";
    };
    "nixbox" = {
      id = "CUSYAW3-XKCSZ3N-3TMMBTQ-OGI37SE-DIOUHUS-KGFS2SX-WWKL2IQ-I3OLTQ2";
    };
  };
  deviceNames = builtins.attrNames devices;
  commonIgnores = [
    ".git"
    ".DS_Store"
    "**/.git/**"
  ];

  requiredFolders = [
    "screenshots@nyeong-air"
    "hanassig"
    "dotfiles"
  ];
  missingFolders = lib.filter (f: !(cfg.folders ? ${f})) requiredFolders;
in {
  options.features.syncthing = {
    enable = lib.mkEnableOption "syncthing feature";

    folders = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      description = "Syncthing folder paths for this host";
      example = {
        "screenshots@nyeong-air" = "/Users/nyeong/Pictures/Screenshots";
        hanassig = "/Users/nyeong/Projects/hanassig";
        dotfiles = "/Users/nyeong/.dotfiles";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = missingFolders == [];
        message = ''
          Syncthing feature: Missing required folder paths: ${lib.concatStringsSep ", " missingFolders}
          Required folders: ${lib.concatStringsSep ", " requiredFolders}
        '';
      }
    ];
    services.syncthing = {
      enable = true;
      guiAddress = "localhost:8384";
      settings = {
        devices = devices;
        folders = {
          "screenshots@nyeong-air" = {
            id = "screenshots@nyeong-air";
            path = cfg.folders."screenshots@nyeong-air";
            devices = deviceNames;
          };
          hanassig = {
            id = "hanassig";
            path = cfg.folders.hanassig;
            devices = deviceNames;
            ignores = commonIgnores;
          };

          dotfiles = {
            id = "dotfiles";
            path = cfg.folders.dotfiles;
            devices = deviceNames;
            ignores = commonIgnores;
          };
        };
      };
    };
  };
}
