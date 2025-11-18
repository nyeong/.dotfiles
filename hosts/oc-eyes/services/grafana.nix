# Grafana
{pkgs, ...}: {
  services.grafana = {
    enable = true;
    settings.server = {
      http_addr = "0.0.0.0";
      http_port = 3000;
      enable_gzip = true;
    };
    declarativePlugins = with pkgs.grafanaPlugins; [];
    provision = {
      enable = true;
      datasources.settings.datasources = [];
    };
  };
}
