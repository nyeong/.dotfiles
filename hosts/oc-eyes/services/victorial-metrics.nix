# Victorial Metrics
{palette, ...}: let
  port = palette.oc-eyes.ports.victoria-metrics;
in {
  services.victoriametrics = {
    enable = true;
    listenAddress = "0.0.0.0:${toString port}";
    retentionPeriod = "50y";
  };
}
