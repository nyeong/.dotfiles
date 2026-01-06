# Path utility functions
{lib}: {
  # Scan a directory and return paths to all .nix files (except default.nix) and directories
  # Usage: scanPaths ./some/path -> [./some/path/foo.nix ./some/path/bar]
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
}
