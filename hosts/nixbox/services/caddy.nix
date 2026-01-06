# Caddy
#
# Tailscale MagicDNS
# ------------------
#
# Tailscale MagicDNS의 한계로, subdomain은 이용 불가. subpath를 이용해야함.
# subpath가 필요한 경우 Tailscale Service로 분리해야함.
{
  palette,
  pkgs,
  config,
  ...
}: let
  nixbox = palette.nixbox;
  services = nixbox.services;
in {
  sops.secrets.webdav_password_hash = {
    sopsFile = ../../../secrets/nixbox.yaml;
    key = "webdav_password_hash";
    mode = "0400";
    owner = "caddy";
  };

  services.caddy = {
    email = palette.user.email;
    package = pkgs.caddy.withPlugins {
      plugins = ["github.com/mholt/caddy-webdav"];
      hash = "sha256-RfoWigQXCh1DVHDLlux1BvJwk3ATWgfODNbkdX35354=";
    };
    enable = true;
    globalConfig = ''
      order webdav before file_server
    '';
    virtualHosts."${nixbox.url}" = {
      serverAliases = [];
      useACMEHost = null;
      listenAddresses = [];
      extraConfig = ''
        # WebDAV for BeOrg sync
        handle /webdav* {
          basic_auth {
            ${palette.user.username} {file.${config.sops.secrets.webdav_password_hash.path}}
          }
          rewrite /webdav /webdav/
          webdav /webdav/* {
            root /srv/hanassig
            prefix /webdav
          }
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
