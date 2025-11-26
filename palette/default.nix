{
  lib,
  systems,
  ...
}: let
  magicdns = "dolly-inanga.ts.net";
  mkMagicDnsUrl = subdomain: "${subdomain}.${magicdns}";

  # Shared library functions
  paletteLib = {
    isDarwin = system: lib.strings.hasSuffix "-darwin" system;
    isLinux = system: lib.strings.hasSuffix "-linux" system;
    mkMagicDnsUrl = mkMagicDnsUrl;
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
    # Conditionally import _${name}.nix if it exists, then merge with default
    # Usage: mkOptionalImport "services" ./. { default = "value"; }
    mkOptionalImport = name: dir: default:
      (
        if builtins.pathExists (dir + "/_${name}.nix")
        then import (dir + "/_${name}.nix")
        else {}
      )
      // default;
  };
in {
  # variables
  user = import ./user-config.nix {inherit lib;};
  nixbox = import ./nixbox {
    inherit mkMagicDnsUrl;
    lib = paletteLib;
  };
  oc-eyes = import ./oc-eyes {inherit mkMagicDnsUrl;};
  tailscale = {
    inherit magicdns;
  };

  # functions
  lib = paletteLib;
}
