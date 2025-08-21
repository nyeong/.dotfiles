{
  pkgs,
  config,
  ...
}: let
  palette = import ../../palette.nix;
  caddy = pkgs.callPackage ./overlay/caddy-tailscale.nix {};
in {
  services.caddy = {
    package = caddy;
    enable = true;
    globalConfig = ''
      tailscale {
        auth_key {file.${config.age.secrets."tailscale.authkey".path}}
        ephemeral true
      }
    '';
    virtualHosts = {
      ":80" = {
        extraConfig = ''
          bind tailscale/nixbox

          # WebDAV reverse proxy
          handle_path /webdav/storage* {
            reverse_proxy localhost:${palette.ports.webdav_storage} {
              header_up Host {upstream_hostport}
              header_up X-Real-IP {remote_host}
              header_up X-Forwarded-For {remote_host}
              header_up X-Forwarded-Proto {scheme}
            }
          }

          handle_path /webdav/hanassig* {
            reverse_proxy localhost:${palette.ports.webdav_hanassig} {
              header_up Host {upstream_hostport}
              header_up X-Real-IP {remote_host}
              header_up X-Forwarded-For {remote_host}
              header_up X-Forwarded-Proto {scheme}
            }
          }

          # 기본 응답
          respond "Hello from nixbox via Tailscale!" 200
        '';
      };
      ":443" = {
        extraConfig = ''
          bind tailscale/nixbox
          tls {
            get_certificate tailscale
          }

          # WebDAV reverse proxy
          handle_path /webdav/storage* {
            reverse_proxy localhost:${palette.ports.webdav_storage} {
              header_up Host {upstream_hostport}
              header_up X-Real-IP {remote_host}
              header_up X-Forwarded-For {remote_host}
              header_up X-Forwarded-Proto {scheme}
            }
          }

          handle_path /webdav/hanassig* {
            reverse_proxy localhost:${palette.ports.webdav_hanassig} {
              header_up Host {upstream_hostport}
              header_up X-Real-IP {remote_host}
              header_up X-Forwarded-For {remote_host}
              header_up X-Forwarded-Proto {scheme}
            }
          }

          # 기본 응답
          respond "Hello from nixbox via Tailscale!" 200
        '';
      };
    };
  };
}
