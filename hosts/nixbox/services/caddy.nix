{pkgs, ...}: let
  tailscaleDomain = "nixbox.dolly-inanga.ts.net";
in {
  # reverse proxies
  services.tailscale.enable = true;
  services.tailscale.permitCertUid = "caddy";
  services.caddy = {
    enable = true;
    virtualHosts."${tailscaleDomain}".extraConfig = ''
      # Default route - simple hello message
      handle {
        respond "Hello from nixbox! Available services: /kavita, /webdav, /filebrowser, /calibre-web"
      }

      # Service routes
      handle /kavita/* {
        uri strip_prefix /kavita
        reverse_proxy localhost:5000
      }

      handle /webdav/* {
        uri strip_prefix /webdav
        reverse_proxy localhost:8080
      }

      handle /filebrowser/* {
        uri strip_prefix /filebrowser
        reverse_proxy localhost:8081
      }

      handle /calibre-web/* {
        uri strip_prefix /calibre-web
        reverse_proxy localhost:8083
      }
    '';
  };
}
