# paperless
{
  palette,
  lib,
  ...
}: let
  cfg = palette.nixbox.services;
in {
  services.paperless = {
    enable = true;
    address = "0.0.0.0";
    port = cfg.paperless.port;
    settings = {
      PAPERLESS_DBENGINE = "postgresql";
      PAPERLESS_DBPORT = cfg.postgres.port;
      PAPERLESS_DBNAME = cfg.paperless.dbname;
      PAPERLESS_DBUSER = cfg.paperless.dbuser;
    };
  };
}
