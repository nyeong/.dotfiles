# Victorial Metrics
{palette, ...}: let
  svc = palette.oc-eyes.services;
in {
  services.victoriametrics = {
    enable = true;
    listenAddress = "0.0.0.0:${toString svc.victoria-metrics.port}";
    retentionPeriod = "50y";
    extraOptions = [
      "-http.pathPrefix=/${svc.victoria-metrics.subpath}"
    ];
  };
}
