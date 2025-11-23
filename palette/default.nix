{
  lib,
  systems,
  ...
}: let
  magicdns = "dolly-inanga.ts.net";
in {
  # variables
  user = import ./user-config.nix {inherit lib;};
  nixbox = import ./nixbox {inherit lib;};
  oc-eyes = import ./oc-eyes {inherit magicdns;};
  tailscale = {
    inherit magicdns;
  };

  # functions
  lib = {
    isDarwin = system: lib.strings.hasSuffix "-darwin" system;
    isLinux = system: lib.strings.hasSuffix "-linux" system;
    mkMagicDnsUrl = subdomain: "${subdomain}.${magicdns}";
    mkPerSystemConfig = import ./per-system-config.nix;
    mkDarwinSystem = import ./darwin-system.nix;
    forAllSystems = func: (lib.genAttrs systems func);
    scanPaths = path:
      builtins.map (f: (path + "/${f}")) (
        builtins.attrNames (
          lib.attrsets.filterAttrs (
            path: _type:
              (_type == "directory") # include directories
              || (
                (path != "default.nix") # ignore default.nix
                && (lib.strings.hasSuffix ".nix" path) # include .nix files
              )
          ) (builtins.readDir path)
        )
      );
  };
}
