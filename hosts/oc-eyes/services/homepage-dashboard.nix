{
  lib,
  palette,
  ...
}: let
  port = palette.oc-eyes.ports.homepage;
  oc-eyes-url = palette.lib.mkMagicDnsUrl "oc-eyes";
in {
  services.homepage-dashboard = {
    enable = true;
    settings = {};
    listenPort = port;
    allowedHosts = lib.concatStringsSep "," [
      oc-eyes-url
    ];
  };
}
