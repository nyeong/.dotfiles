# Host-specific data configurations
{utils}: {
  nixbox = import ./nixbox.nix {inherit utils;};
  oc-eyes = import ./oc-eyes.nix {inherit utils;};
}
