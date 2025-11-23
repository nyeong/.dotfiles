{
  lib,
  palette,
  config,
  ...
}: let
  port = palette.oc-eyes.ports.homepage;
  oc-eyes-url = palette.lib.mkMagicDnsUrl "oc-eyes";
  reverse-proxy-path = palette.oc-eyes.reverse-proxy.homepage;
  base-url = "https://${oc-eyes-url}/${reverse-proxy-path}";
in {
  services.homepage-dashboard = {
    enable = true;
    settings = {
      startUrl = base-url;
      base = base-url;
    };
    listenPort = port;
    allowedHosts = lib.concatStringsSep "," [
      oc-eyes-url
    ];
  };
}
