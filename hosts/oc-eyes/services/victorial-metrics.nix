# Victorial Metrics
{palette, ...}: let
  cfg = palette.oc-eyes;
in {
  services.victoriametrics = {
    enable = true;
    listenAddress = "0.0.0.0:${toString cfg.services.victoria-metrics.port}";
    retentionPeriod = "50y";
    extraOptions = [
      "-http.pathPrefix=/${cfg.services.victoria-metrics.subpath}"
    ];
  };
}
