# Grafana
# Use Caddy as reverse proxy
{
  pkgs,
  palette,
  ...
}: let
  svc = palette.oc-eyes.services;
  domain = palette.lib.mkMagicDnsUrl "oc-eyes";
in {
  services.grafana = {
    enable = true;
    settings.server = {
      http_addr = "0.0.0.0";
      http_port = svc.grafana.port;
      enable_gzip = true;
      domain = domain;
      root_url = "https://${domain}/${svc.grafana.subpath}/";
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
          url = "http://localhost:${toString svc.victoria-metrics.port}";
        }
      ];
    };
  };
}
