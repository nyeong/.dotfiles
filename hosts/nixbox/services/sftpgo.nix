{
  lib,
  config,
  palette,
  ...
}: let
  cfg = palette.nixbox.services;
in {
  services.sftpgo = {
    enable = true;
    dataDir = "/var/lib/sftpgo";
    settings = {
      data_provider = {
        driver = "postgresql";
        name = cfg.sftpgo.dbname;
        host = "/run/postgresql";
        port = 5432;
        username = cfg.sftpgo.dbuser;
        ssl_mode = 0;
      };
      httpd.bindings = [
        {
          port = cfg.sftpgo.port;
          address = "0.0.0.0";
        }
      ];
      webdavd.bindings = [
        {
          port = cfg.webdav.port;
          address = "0.0.0.0";
        }
      ];
    };
  };

  systemd.services.sftpgo = {
    serviceConfig.SupplementaryGroups = ["share"];
    after = ["postgresql.service"];
    wants = ["postgresql.service"];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  users.users.sftpgo.extraGroups = ["share"];
}
