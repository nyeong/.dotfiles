# vmagent
# 메트릭 수집 및 전송
{
  palette,
  config,
  ...
}: {
  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = ["systemd"];
    port = 9100;
  };

  services.vmagent = {
    enable = true;
    remoteWrite.url = "http://${palette.oc-eyes.host}:${toString palette.oc-eyes.ports.victoria-metrics}/api/v1/write";
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
