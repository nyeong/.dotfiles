# Caddy
#
# Tailscale MagicDNS
# ------------------
#
# Tailscale MagicDNS의 한계로, subdomain은 이용 불가. subpath를 이용해야함.
# subpath가 필요한 경우 Tailscale Service로 분리해야함.
{palette, ...}: let
  nixbox = palette.nixbox;
  services = nixbox.services;
in {
  services.caddy = {
    email = palette.user.email;
    enable = true;
    virtualHosts."${nixbox.url}" = {
      serverAliases = [];
      useACMEHost = null;
      listenAddresses = [];
      extraConfig = ''
        # Default handler - catches all unmatched requests
        handle /webdav {
          redir /webdav/ permanent
        }

        handle_path /webdav/* {
          reverse_proxy http://localhost:${toString services.webdav.port}
        }

        handle_path /${services.filebrowser.subpath}* {
          reverse_proxy http://localhost:${toString services.filebrowser.port}
        }

        handle {
          respond "404 Not Found" 404
        }
      '';
    };
  };
}
