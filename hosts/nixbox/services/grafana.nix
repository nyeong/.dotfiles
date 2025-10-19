{lib, ...}: let
  palette = import ../_palette.nix;
in {
  users.users.grafana.extraGroups = ["postgres"];
  services.grafana = {
    enable = true;
    settings.server = {
      http_port = lib.toInt palette.ports.grafana;
      http_addr = "0.0.0.0";
    };
  };
}
