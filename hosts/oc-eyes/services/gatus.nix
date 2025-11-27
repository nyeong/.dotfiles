# Gatus - Health Check & Status Page
{
  config,
  lib,
  pkgs,
  palette,
  ...
}: let
  cfg = palette.oc-eyes;
  tailscaleBin = "${pkgs.tailscale}/bin/tailscale";
  gatusPort = toString cfg.services.gatus.port;
  gatusService = cfg.services.gatus.subdomain;
in {
  services.gatus = {
    enable = true;
    settings = {
      web = {
        port = cfg.services.gatus.port;
        address = "0.0.0.0";
      };

      storage = {
        type = "sqlite";
        path = "/var/lib/gatus/data.db";
      };

      # Alerting configuration (optional)
      alerting = {
        discord = null;
        slack = null;
      };

      # Endpoints to monitor
      endpoints = [
        {
          name = "Homepage";
          url = "https://${cfg.url}/";
          interval = "30s";
          conditions = [
            "[STATUS] == 200"
            "[RESPONSE_TIME] < 1000"
          ];
          alerts = [];
        }
        {
          name = "Grafana";
          url = "https://${cfg.url}/${cfg.services.grafana.subpath}/";
          interval = "30s";
          conditions = [
            "[STATUS] == 200"
            "[RESPONSE_TIME] < 2000"
          ];
          alerts = [];
        }
        {
          name = "VictoriaMetrics";
          url = "https://${cfg.url}/${cfg.services.victoria-metrics.subpath}/";
          interval = "30s";
          conditions = [
            "[STATUS] == 200"
            "[RESPONSE_TIME] < 1000"
          ];
          alerts = [];
        }
      ];
    };
  };

  systemd.services.tailscale-serve-gatus = palette.lib.mkTailscaleServeService {
    inherit tailscaleBin;
    serviceName = gatusService;
    port = cfg.services.gatus.port;
    webService = "gatus.service";
    enable = config.services.gatus.enable;
  };

  # Ensure data directory exists
  systemd.tmpfiles.rules = [
    "d /var/lib/gatus 0750 gatus gatus -"
  ];
}
