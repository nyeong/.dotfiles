# Gatus - Health Check & Status Page
{palette, ...}: let
  svc = palette.oc-eyes.services;
  oc-eyes-url = palette.lib.mkMagicDnsUrl "oc-eyes";
in {
  services.gatus = {
    enable = true;
    settings = {
      web = {
        port = svc.gatus.port;
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
          url = "https://${oc-eyes-url}/";
          interval = "30s";
          conditions = [
            "[STATUS] == 200"
            "[RESPONSE_TIME] < 1000"
          ];
          alerts = [];
        }
        {
          name = "Grafana";
          url = "https://${oc-eyes-url}/${svc.grafana.subpath}/";
          interval = "30s";
          conditions = [
            "[STATUS] == 200"
            "[RESPONSE_TIME] < 2000"
          ];
          alerts = [];
        }
        {
          name = "VictoriaMetrics";
          url = "https://${oc-eyes-url}/${svc.victoria-metrics.subpath}/";
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

  # Ensure data directory exists
  systemd.tmpfiles.rules = [
    "d /var/lib/gatus 0750 gatus gatus -"
  ];
}
