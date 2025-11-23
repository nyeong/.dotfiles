# MagincDNS Caddy server
{palette, ...}: let
  magicdns-url = palette.lib.mkMagicDnsUrl "oc-eyes";
  ports = palette.oc-eyes.ports;
  reverse-proxy = palette.oc-eyes.reverse-proxy;
in {
  services.caddy = {
    email = palette.user.email;
    enable = true;
    virtualHosts."${magicdns-url}" = {
      serverAliases = [];
      useACMEHost = null;
      listenAddresses = [];
      extraConfig = ''
        handle /${reverse-proxy.grafana}* {
          reverse_proxy http://localhost:${toString ports.grafana} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
            header_up X-Forwarded-Prefix /${reverse-proxy.grafana}
            header_up Connection {>Connection}
            header_up Upgrade {>Upgrade}
          }
        }

        handle_path /${reverse-proxy.uptime-kuma} {
          reverse_proxy http://localhost:${toString ports.uptime-kuma} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
            header_up X-Forwarded-Prefix /${reverse-proxy.uptime-kuma}
          }
        }

        handle_path /${reverse-proxy.homepage} {
          reverse_proxy http://localhost:${toString ports.homepage} {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
            header_up X-Forwarded-Prefix /${reverse-proxy.homepage}
          }
        }

        handle {
          redir / /${reverse-proxy.homepage}
        }
      '';
    };
  };
}
