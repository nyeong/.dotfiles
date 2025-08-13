{...}: let
  tailscaleDomain = "nixbox.dolly-inanga.ts.net";
in {
  # reverse proxies
  services.tailscale.enable = true;
  services.caddy = {
    enable = true;
    virtualHosts."kavita.${tailscaleDomain}".extraConfig = ''
      reverse_proxy localhost:5000
    '';
    virtualHosts."filebrowser.${tailscaleDomain}".extraConfig = ''
      reverse_proxy localhost:8081
    '';
    virtualHosts."calibre-web.${tailscaleDomain}".extraConfig = ''
      reverse_proxy localhost:5000
    '';
  };
}
