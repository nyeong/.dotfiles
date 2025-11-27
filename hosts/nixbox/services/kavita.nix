{
  palette,
  config,
  lib,
  pkgs,
  ...
}: let
  nixbox = palette.nixbox;
  kavita = nixbox.services.kavita;
  tailscaleBin = "${pkgs.tailscale}/bin/tailscale";
in {
  sops.secrets.kavita_token = {
    sopsFile = ../../../secrets/nixbox.yaml;
    key = "kavita_token";
    mode = "0400";
  };

  services.kavita = {
    enable = true;
    tokenKeyFile = config.sops.secrets.kavita_token.path;
    settings = {
      Port = kavita.port;
      HostName = "https://${kavita.url}";
    };
  };

  systemd.services.tailscale-serve-kavita = palette.lib.mkTailscaleServeService {
    inherit tailscaleBin;
    serviceName = kavita.serviceName;
    port = kavita.port;
    webService = "kavita.service";
    enable = config.services.kavita.enable;
  };
}
