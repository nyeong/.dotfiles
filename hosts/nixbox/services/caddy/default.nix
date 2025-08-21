{
  pkgs,
  config,
  ...
}: let
  palette = import ../../_palette.nix;
  caddy = pkgs.callPackage ./overlay/caddy-tailscale.nix {};
in {
  services.caddy = {
    package = caddy;
    enable = true;
    globalConfig = ''
      tailscale {
        auth_key ${config.age.secrets."tailscale.nixbox.auth".path}
        ephemeral true
      }
    '';
    virtualHosts = {
      "https://webdav.dolly-inanga.ts.net" = {
        extraConfig = ''
          bind tailscale/webdav

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
