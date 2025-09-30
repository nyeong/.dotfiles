{lib, ...}: let
  palette = import ../../_palette.nix;
in {
  services.adguardhome = {
    enable = true;
    host = "0.0.0.0";
    port = lib.toInt palette.ports.adguard;
    settings = {
      dns = {
        # Bind only to loopback, LAN, and Tailscale; avoid Podman bridge (10.88.0.1)
        bind_hosts = ["127.0.0.1" "192.168.1.160" "100.64.60.80"];
        # Upstreams priority: Google first (h3 DoH -> DoH -> DoQ -> DoT), then Cloudflare
        # Ref: AdGuard upstream syntax (h3/tls/quic/https)
        upstream_dns = [
          # Google
          "h3://dns.google/dns-query"
          "https://dns.google/dns-query"
          "quic://dns.google"
          "tls://dns.google"
          # Cloudflare
          "h3://cloudflare-dns.com/dns-query"
          "https://cloudflare-dns.com/dns-query"
          "quic://cloudflare-dns.com"
          "tls://cloudflare-dns.com"
        ];
        # Bootstrap to resolve DoH hostnames at startup
        bootstrap_dns = ["1.1.1.1" "1.0.0.1" "8.8.8.8" "8.8.4.4"];
        # Enable DNSSEC validation
        dnssec_enabled = true;
      };
    };
  };
}
