# MagincDNS Caddy server
{palette, ...}: let
  magicdns-url = palette.lib.mkMagicDnsUrl "oc-eyes";
  ports = palette.oc-eyes.ports;
in {
  services.caddy = {
    email = palette.user.email;
    enable = true;
    virtualHosts."${magicdns-url}" = {
      serverAliases = [];
      useACMEHost = null;
      listenAddresses = [];
      extraConfig = ''
        handle /monitor* {
          reverse_proxy http://localhost:${toString ports.grafana} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
            header_up X-Forwarded-Prefix /monitor
            header_up Connection {>Connection}
            header_up Upgrade {>Upgrade}
          }
        }

        handle_path /uptime {
          reverse_proxy http://localhost:${toString ports.uptime-kuma} {
            header_up Host {host}
            header_up X-Real-IP {remote}
            header_up X-Forwarded-Prefix /uptime
          }
        }

        handle_path /dashboard {
          reverse_proxy http://localhost:${toString ports.homepage}
        }

        handle {
          redir / /dashboard
        }
      '';
    };
  };
}
