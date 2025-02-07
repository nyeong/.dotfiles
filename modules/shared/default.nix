{ config, pkgs, ... }:

let
  emacsOverlaySha256 = "0jlddbg2x6g6gi9aimd6ycxl86ndxd9lg3mz0xmyq174qmqij218";
in
{
  services.emacs.package = pkgs.emacs-unstable;

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };

    overlays =
      # Apply each overlay found in the /overlays directory
      let path = ../../overlays; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))

      ++ [(import (builtins.fetchTarball {
               url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
               sha256 = emacsOverlaySha256;
           }))];
  };
}
