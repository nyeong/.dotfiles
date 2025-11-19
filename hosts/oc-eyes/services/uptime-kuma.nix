# uptime kuma
{palette, ...}: let
  port = palette.oc-eyes.ports.uptime-kuma;
in {
  services.uptime-kuma = {
    enable = true;
    settings = {
      HOST = "0.0.0.0";
      PORT = toString port;
    };
  };
}
