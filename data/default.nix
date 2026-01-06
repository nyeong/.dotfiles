# Data - Pure values and configurations
# Layer 4: No dependencies on other layers (except utils for host configs)
{
  lib,
  utils,
}: let
  tailscale = import ./tailscale.nix;
  mkUrl = utils.mkMagicDnsUrl tailscale.magicdns;
  # Create utils with bound magicdns and mkOptionalImport
  boundUtils =
    utils
    // {
      mkMagicDnsUrl = mkUrl;
      mkOptionalImport = utils.mkOptionalImport;
    };
in {
  # User information
  user = import ./user.nix;

  # Tailscale configuration
  inherit tailscale;

  # Host-specific configurations
  nixbox = import ./hosts/nixbox.nix {utils = boundUtils;};
  oc-eyes = import ./hosts/oc-eyes.nix {utils = boundUtils;};

  # Agent configurations
  agents = import ./agents;
}
