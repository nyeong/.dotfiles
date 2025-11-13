{
  config,
  secrets,
  palette,
  ...
}: let
  nixbox = palette.nixbox;
  tunnelId = "b5ba47b0-5a2e-4bc3-bdc7-7d88df40d0a7";
in {
  age.secrets."cf.tunnel.nyeong_me.json" = {
    file = "${secrets}/cf.tunnel.nyeong_me.json.age";
    owner = "nyeong";
    group = "users";
    mode = "0400";
  };

  age.secrets."cf.tunnel.cert" = {
    file = "${secrets}/cf.tunnel.cert.age";
    owner = "nyeong";
    group = "users";
    mode = "0400";
  };

  services.cloudflared = {
    enable = true;
    tunnels = {
      ${tunnelId} = {
        credentialsFile = config.age.secrets."cf.tunnel.nyeong_me.json".path;
        ingress = {
          "${nixbox.network.domain.immich-service}" = "http://localhost:${nixbox.network.ports.immich}";
        };
        default = "http_status:404";
      };
    };
  };

  systemd.services."cloudflared-tunnel-${tunnelId}" = {
    environment = {
      TUNNEL_ORIGIN_CERT = config.age.secrets."cf.tunnel.cert".path;
    };
  };
}
