# MagincDNS Caddy server
{palette, ...}: let
  cfg = palette.oc-eyes;
in {
  services.caddy = {
    email = palette.user.email;
    enable = true;
    virtualHosts."${cfg.url}" = {
      serverAliases = [];
      useACMEHost = null;
      listenAddresses = [];
      extraConfig = ''
        encode zstd gzip

        handle /${cfg.services.grafana.subpath}* {
          reverse_proxy http://localhost:${toString cfg.services.grafana.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
            header_up X-Forwarded-Prefix /${cfg.services.grafana.subpath}
            header_up Connection {>Connection}
            header_up Upgrade {>Upgrade}
          }
        }

        handle_path /${cfg.services.gatus.subpath}* {
          reverse_proxy http://localhost:${toString cfg.services.gatus.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
          }
        }

        handle /${cfg.services.victoria-metrics.subpath}* {
          reverse_proxy http://localhost:${toString cfg.services.victoria-metrics.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
          }
        }

        # Homepage at root - catches all unmatched requests
        handle {
          reverse_proxy http://localhost:${toString cfg.services.homepage.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
          }
        }
      '';
    };
  };
}
