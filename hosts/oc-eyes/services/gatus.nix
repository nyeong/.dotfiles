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

  systemd.services.tailscale-serve-gatus = lib.mkIf config.services.gatus.enable {
    description = "Expose Gatus via Tailscale Serve";
    requires = [
      "tailscaled.service"
      "gatus.service"
    ];
    after = [
      "tailscaled.service"
      "gatus.service"
    ];
    wantedBy = ["multi-user.target"];
    partOf = [
      "tailscaled.service"
      "gatus.service"
    ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = ''
        ${tailscaleBin} serve --service=svc:${gatusService} --https=443 127.0.0.1:${gatusPort}
      '';
      ExecStop = ''
        ${tailscaleBin} serve drain svc:${gatusService}
        ${tailscaleBin} serve --service=svc:${gatusService} --https=443 off
        ${tailscaleBin} serve clear svc:${gatusService}
      '';
    };
  };

  # Ensure data directory exists
  systemd.tmpfiles.rules = [
    "d /var/lib/gatus 0750 gatus gatus -"
  ];
}
