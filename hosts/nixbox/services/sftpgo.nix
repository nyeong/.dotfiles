{
  lib,
  config,
  palette,
  ...
}: let
  nixbox = palette.nixbox;
in {
  services.sftpgo = {
    enable = true;
    dataDir = "/var/lib/sftpgo";
    settings = {
      data_provider = {
        driver = "postgresql";
        name = "sftpgo";
        host = "/run/postgresql";
        port = 5432;
        username = "sftpgo";
        ssl_mode = 0;
      };
      httpd = {
        bindings = [
          {
            port = lib.toInt nixbox.network.ports.sftpgo;
            address = "0.0.0.0";
          }
        ];
      };
      webdavd = {
        bindings = [
          {
            port = 10080;
            address = "0.0.0.0";
          }
        ];
      };
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
}
