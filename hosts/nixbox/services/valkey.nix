# Valkey - Redis-compatible cache
{
  config,
  pkgs,
  lib,
  palette,
  ...
}: let
  cfg = palette.nixbox.services;
in {
  services.redis = {
    package = pkgs.valkey;
    servers."default" = {
      enable = true;
      bind = "0.0.0.0";
      port = cfg.valkey.port;
      maxclients = 10000;
    };
  };
}
