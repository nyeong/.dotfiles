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
    user = kavita.user;
    settings = {
      Port = kavita.port;
      HostName = "https://${kavita.url}";
    };
  };

  users.users.${kavita.user} = {
    extraGroups = ["share"];
  };

  systemd.tmpfiles.rules = [
    "d /storage/@library 0755 root share -"
    "d /storage/@library/books 0775 root share -"
    "d /storage/@library/comics 0775 root share -"
    "d /storage/@library/lectures 0775 root share -"
    "d /storage/@library/papers 0775 root share -"
    "d /storage/@library/scores 0775 root share -"
    "d /storage/@library/talks 0775 root share -"
  ];

  systemd.services.tailscale-serve-kavita = palette.lib.mkTailscaleServeService {
    inherit tailscaleBin;
    serviceName = kavita.serviceName;
    port = kavita.port;
    webService = "kavita.service";
    enable = config.services.kavita.enable;
  };
}
