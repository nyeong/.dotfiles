{
  lib,
  pkgs,
  config,
  palette,
  ...
}: let
  cfg = palette.nixbox.services;
in {
  # services.pgadmin = {
  #   enable = true;
  # };
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_18;
    extensions = ps:
      with ps; [
        # timescaledb
        # timescaledb_toolkit
      ];
    dataDir = "/var/lib/postgresql";

    ensureDatabases = [
      cfg.paperless.dbname
      cfg.sftpgo.dbname
    ];
    ensureUsers = [
      {
        name = cfg.paperless.dbuser;
        ensureDBOwnership = true;
      }
      {
        name = cfg.sftpgo.dbuser;
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
    "d /var/lib/postgresql 0700 postgres postgres -"
  ];
}
