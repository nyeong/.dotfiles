{palette, ...}: {
  imports = palette.lib.scanPaths ./.;
  nix.gc.dates = "weekly";
}
