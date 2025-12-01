{palette, ...}: let
  calibre-web = palette.nixbox.services.calibre-web;
in {
  services.calibre-web = {
    enable = true;
    listen.port = calibre-web.port;
    listen.ip = "0.0.0.0";
  };
}
