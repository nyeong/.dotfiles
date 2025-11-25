# Grafana
# Use Caddy as reverse proxy
{
  pkgs,
  palette,
  ...
}: let
  cfg = palette.oc-eyes;
in {
  services.grafana = {
    enable = true;
    settings.server = {
      http_addr = "0.0.0.0";
      http_port = cfg.services.grafana.port;
      enable_gzip = true;
      domain = cfg.url;
      root_url = "https://${cfg.url}/${cfg.services.grafana.subpath}/";
      serve_from_sub_path = true;
      protocol = "http";
    };
    declarativePlugins = with pkgs.grafanaPlugins; [];
    provision = {
      enable = true;
      datasources.settings.datasources = [
        {
          name = "VictoriaMetrics";
          type = "prometheus";
          url = "http://localhost:${toString cfg.services.victoria-metrics.port}/${cfg.services.victoria-metrics.subpath}";
        }
        {
          name = "VictoriaLogs";
          type = "loki";
          url = "http://localhost:${toString cfg.services.victoria-logs.port}/${cfg.services.victoria-logs.subpath}";
          jsonData = {
            maxLines = 1000;
          };
        }
      ];
    };
  };
}
