{
  pkgs,
  config,
  secrets,
  ...
}: let
  palette = import ../../_palette.nix;
  caddy = pkgs.callPackage ./overlay/caddy-tailscale.nix {};
in {
  age.secrets."tailscale.nixbox.auth" = {
    file = "${secrets}/tailscale.nixbox.auth.age";
    owner = config.services.caddy.user;
    group = config.services.caddy.group;
    mode = "0600";
  };

  services.caddy = {
    package = caddy;
    enable = true;
    globalConfig = ''
      tailscale {
        ephemeral true
      }
    '';
    virtualHosts = {
      "https://webdav.${palette.tailscale.tailnet_name}" = {
        extraConfig = ''
          bind tailscale/webdav

          # WebDAV reverse proxy
          handle_path /storage/* {
            @options method OPTIONS
            handle @options {
              header DAV "1, 2"
              header MS-Author-Via "DAV"
              header Allow "OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, COPY, MOVE, MKCOL, PROPFIND, PROPPATCH, LOCK, UNLOCK"
              header Cache-Control "no-cache"
              respond "" 200
            }
            reverse_proxy localhost:${palette.ports.webdav_storage} {
              header_up Host {upstream_hostport}
              header_up X-Real-IP {remote_host}
              header_up X-Forwarded-For {remote_host}
              header_up X-Forwarded-Proto {scheme}
            }
          }


          handle_path /hanassig/* {
            @options method OPTIONS
            handle @options {
              header DAV "1, 2"
              header MS-Author-Via "DAV"
              header Allow "OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, COPY, MOVE, MKCOL, PROPFIND, PROPPATCH, LOCK, UNLOCK"
              header Cache-Control "no-cache"
              respond "" 200
            }
            reverse_proxy localhost:${palette.ports.webdav_hanassig} {
              transport http {
                versions 1.1
              }

              header_up Host {upstream_hostport}
              header_up X-Real-IP {remote_host}
              header_up X-Forwarded-For {remote_host}
              header_up X-Forwarded-Proto {scheme}

              # iOS WebDAV 호환성 개선
              flush_interval -1
              header_down -Transfer-Encoding
              header_down DAV "1, 2"
              header_down MS-Author-Via "DAV"
              header_down Allow "OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, COPY, MOVE, MKCOL, PROPFIND, PROPPATCH, LOCK, UNLOCK"
              header_down Cache-Control "no-cache"
            }
          }

          # 기본 응답
          respond "Hello from nixbox via Tailscale!" 200
        '';
      };
    };
  };

  systemd.services.caddy.serviceConfig.EnvironmentFile = config.age.secrets."tailscale.nixbox.auth".path;
  systemd.services.caddy.serviceConfig.ExecReload = "${pkgs.coreutils}/bin/kill -TERM $MAINPID";
}
