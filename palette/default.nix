{
  lib,
  systems,
  ...
}: let
  magicdns = "dolly-inanga.ts.net";
  mkMagicDnsUrl = subdomain: "${subdomain}.${magicdns}";

  # Tailscale Serve helper function
  # Returns a systemd service configuration that can be assigned directly
  mkTailscaleServeService = {
    tailscaleBin,
    serviceName,
    port,
    webService,
    enable ? true,
  }:
    lib.mkIf enable {
      description = "Expose ${serviceName} via Tailscale Serve";
      requires = [
        "tailscaled.service"
        "${webService}"
      ];
      after = [
        "tailscaled.service"
        "${webService}"
      ];
      wantedBy = ["multi-user.target"];
      partOf = [
        "tailscaled.service"
        "${webService}"
      ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = [
          "${tailscaleBin} serve --service=svc:${serviceName} --https=443 127.0.0.1:${toString port}"
        ];
        ExecStop = [
          "${tailscaleBin} serve drain svc:${serviceName}"
          "${tailscaleBin} serve --service=svc:${serviceName} --https=443 off"
          "${tailscaleBin} serve clear svc:${serviceName}"
        ];
      };
    };

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
    # Tailscale Serve helper function
    # Usage: palette.lib.mkTailscaleServeService { ... }
    inherit mkTailscaleServeService;
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
  agents = import ./agents {inherit lib;};

  # functions
  lib = paletteLib;
}
