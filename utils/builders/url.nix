# URL builder functions
{
  # Build a MagicDNS URL for Tailscale services
  # Usage: mkMagicDnsUrl magicdns "subdomain" -> "subdomain.ts.net"
  mkMagicDnsUrl = magicdns: subdomain: "${subdomain}.${magicdns}";
}
