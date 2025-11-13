{
  lib,
  config,
  palette,
  ...
}: let
  nixbox = palette.nixbox;
  grafanaPort = lib.toInt nixbox.network.ports.grafana;
in {
  users.users.grafana.extraGroups = ["postgres"];

  services.grafana = {
    enable = true;

    settings = {
      server = {
        http_port = grafanaPort;
        http_addr = "0.0.0.0";
      };

      # Authentication
      "auth.anonymous" = {
        enabled = false;
      };

      # Disable update checks
      analytics = {
        reporting_enabled = false;
        check_for_updates = false;
      };
    };

    # Provision datasources
    provision = {
      datasources.settings.datasources = [
        {
          name = "PostgreSQL";
          type = "postgres";
          access = "proxy";
          url = "localhost:5432";
          database = "telegraf";
          user = "telegraf";
          isDefault = true;
          jsonData = {
            sslmode = "disable";
            postgresVersion = 1600;
          };
        }
      ];
    };
  };
}
