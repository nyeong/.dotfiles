{pkgs, ...}: let
  tailscaleDomain = "nixbox.dolly-inanga.ts.net";
in {
  # reverse proxies
  services.tailscale.enable = true;
  services.tailscale.permitCertUid = "caddy";
  services.caddy = {
    enable = true;
    virtualHosts."${tailscaleDomain}".extraConfig = ''
    '';
  };
}
