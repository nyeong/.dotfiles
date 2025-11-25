{
  config,
  palette,
  ...
}: let
  cfg = palette.nixbox.services;
  tunnelId = "b5ba47b0-5a2e-4bc3-bdc7-7d88df40d0a7";
in {
  sops.secrets."cf.tunnel.nyeong_me.json" = {
    sopsFile = ../../../secrets/nixbox.yaml;
    key = "cf_tunnel_nyeong_me_json";
    owner = "nyeong";
    group = "users";
    mode = "0400";
  };

  sops.secrets."cf.tunnel.cert" = {
    sopsFile = ../../../secrets/nixbox.yaml;
    key = "cf_tunnel_cert";
    owner = "nyeong";
    group = "users";
    mode = "0400";
  };

  services.cloudflared = {
    enable = true;
    tunnels = {
      ${tunnelId} = {
        credentialsFile = config.sops.secrets."cf.tunnel.nyeong_me.json".path;
        ingress = {
          "${cfg.immich.publicUrl}" = "http://localhost:${toString cfg.immich.port}";
        };
        default = "http_status:404";
      };
    };
  };

  systemd.services."cloudflared-tunnel-${tunnelId}" = {
    environment = {
      TUNNEL_ORIGIN_CERT = config.sops.secrets."cf.tunnel.cert".path;
    };
  };
}
