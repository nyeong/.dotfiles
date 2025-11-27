# Caddy
#
# Tailscale MagicDNS
# ------------------
#
# Tailscale MagicDNS의 한계로, subdomain은 이용 불가. subpath를 이용해야함.
# subpath가 필요한 경우 Tailscale Service로 분리해야함.
{palette, ...}: let
  oc-eyes = palette.oc-eyes;
  services = oc-eyes.services;
in {
  services.caddy = {
    email = palette.user.email;
    enable = true;
    virtualHosts."${oc-eyes.url}" = {
      serverAliases = [];
      useACMEHost = null;
      listenAddresses = [];
      extraConfig = ''
        encode zstd gzip

        handle /${services.grafana.subpath}* {
          reverse_proxy http://localhost:${toString services.grafana.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
            header_up X-Forwarded-Prefix /${services.grafana.subpath}
            header_up Connection {>Connection}
            header_up Upgrade {>Upgrade}
          }
        }

        handle /${services.victoria-metrics.subpath}* {
          reverse_proxy http://localhost:${toString services.victoria-metrics.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
          }
        }

        handle /${services.victoria-logs.subpath}* {
          reverse_proxy http://localhost:${toString services.victoria-logs.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
          }
        }

        # Homepage at root - catches all unmatched requests
        handle {
          reverse_proxy http://localhost:${toString services.homepage.port} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
          }
        }
      '';
    };
  };
}
