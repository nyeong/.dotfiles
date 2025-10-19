{
  lib,
  pkgs,
  config,
  ...
}: let
  palette = import ../_palette.nix;
in {
  services.pgadmin = {
    enable = true;
  };
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16;
    extensions = ps:
      with ps; [
        timescaledb
      ];
    dataDir = "/var/lib/postgresql";

    ensureDatabases = ["sftpgo" "grafana"];
    ensureUsers = [
      {
        name = "sftpgo";
        ensureDBOwnership = true;
      }
      {
        name = "grafana";
        ensureDBOwnership = true;
      }
    ];

    settings = {
      max_connections = 100;
      shared_buffers = "2GB";
      effective_cache_size = "24GB";
      maintenance_work_mem = "512MB";
      work_mem = "64MB";
      checkpoint_completion_target = "0.9";
      wal_buffers = "64MB";
      default_statistics_target = "500";
    };
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/postgresql 0755 postgres postgres -"
  ];
}
