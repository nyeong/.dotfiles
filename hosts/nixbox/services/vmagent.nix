# vmagent
# 메트릭 수집 및 전송
{
  palette,
  config,
  ...
}: let
  oc-eyes = palette.oc-eyes;
  cfg = palette.nixbox.services;
in {
  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = ["systemd"];
    port = cfg.vmagent.port;
  };

  services.vmagent = {
    enable = true;
    remoteWrite.url = "https://${oc-eyes.url}/${oc-eyes.services.victoria-metrics.subpath}/api/v1/write";
    prometheusConfig.scrape_configs = [
      {
        job_name = "node-exporter";
        metrics_path = "/metrics";
        static_configs = [
          {
            targets = ["127.0.0.1:${toString config.services.prometheus.exporters.node.port}"];
            labels.type = "node";
            labels.instance = "nixbox";
          }
        ];
      }
    ];
  };
}
