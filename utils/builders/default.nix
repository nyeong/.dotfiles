# Builder functions - pure functions for constructing configurations
{lib}: let
  url = import ./url.nix;
  paths = import ./paths.nix {inherit lib;};
  tailscale = import ./tailscale.nix {inherit lib;};
  system = import ./system.nix;
in {
  inherit (url) mkMagicDnsUrl;
  inherit (paths) scanPaths mkOptionalImport;
  inherit (tailscale) mkTailscaleServeService;
  inherit (system) mkPerSystemConfig;
}
