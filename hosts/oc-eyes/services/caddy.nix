# MagincDNS Caddy server
{palette, ...}: let
  magicdns-url = palette.lib.mkMagicDnsUrl "oc-eyes";
  svc = palette.oc-eyes.services;
in {
  services.caddy = {
    email = palette.user.email;
    enable = true;
    virtualHosts."${magicdns-url}" = {
      serverAliases = [];
      useACMEHost = null;
      listenAddresses = [];
      extraConfig = ''
        encode zstd gzip

        handle /${svc.grafana.subpath}* {
          reverse_proxy http://localhost:${toString svc.grafana.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
            header_up X-Forwarded-Prefix /${svc.grafana.subpath}
            header_up Connection {>Connection}
            header_up Upgrade {>Upgrade}
          }
        }

        handle_path /${svc.uptime-kuma.subpath}* {
          reverse_proxy http://localhost:${toString svc.uptime-kuma.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
            header_up X-Forwarded-Prefix /${svc.uptime-kuma.subpath}
          }
        }

        # Homepage at root - catches all unmatched requests
        handle {
          reverse_proxy http://localhost:${toString svc.homepage.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
          }
        }
      '';
    };
  };
}
