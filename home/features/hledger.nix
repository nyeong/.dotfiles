# https://hledger.org/index.html
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.features.hledger;
in {
  options.features.hledger = {
    enable = lib.mkEnableOption "hledger!";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      hledger
      hledger-ui
      hledger-web
    ];
  };
}
