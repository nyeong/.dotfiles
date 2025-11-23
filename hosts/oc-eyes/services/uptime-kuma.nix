# uptime kuma
{palette, ...}: let
  svc = palette.oc-eyes.services;
in {
  services.uptime-kuma = {
    enable = true;
    settings = {
      HOST = "0.0.0.0";
      PORT = toString svc.uptime-kuma.port;
    };
  };
}
