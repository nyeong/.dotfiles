# Victoria Logs
{palette, ...}: let
  cfg = palette.oc-eyes;
in {
  services.victorialogs = {
    enable = true;
    listenAddress = "0.0.0.0:${toString cfg.services.victoria-logs.port}";
    extraOptions = [
      "-http.pathPrefix=/${cfg.services.victoria-logs.subpath}"
    ];
  };
}
