# Grafana
{
  pkgs,
  palette,
  ...
}: let
  ports = palette.oc-eyes.ports;
in {
  services.grafana = {
    enable = true;
    settings.server = {
      http_addr = "0.0.0.0";
      http_port = ports.grafana;
      enable_gzip = true;
    };
    declarativePlugins = with pkgs.grafanaPlugins; [];
    provision = {
      enable = true;
      datasources.settings.datasources = [
        {
          name = "VictoriaMetrics";
          type = "prometheus";
          url = "http://localhost:${toString ports.victoria-metrics}";
        }
      ];
    };
  };
}
