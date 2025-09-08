{
  config,
  pkgs,
  userConfig,
  lib,
  ...
}: let
  puid = "1000";
  pgid = "100";
  tz = "Asia/Seoul";

  update-containers = pkgs.writeShellScriptBin "update-containers" ''
    SUDO=""
    if [[ $(id -u) -ne 0 ]]; then
    	SUDO="sudo"
    fi

       images=$($SUDO ${pkgs.podman}/bin/podman ps -a --format="{{.Image}}" | sort -u)

       for image in $images
       do
         $SUDO ${pkgs.podman}/bin/podman pull $image
       done
  '';

  # Dynamically import all .nix files in the containers directory
  containerModules = let
    containerDir = ./.;
    files = builtins.readDir containerDir;
    nixFiles =
      lib.filterAttrs (
        name: type:
          type
          == "regular"
          && lib.hasSuffix ".nix" name
          && name != "default.nix"
      )
      files;
    moduleArgs = {
      inherit config pkgs userConfig lib;
      containerConfig = {
        inherit puid pgid tz;
      };
    };
  in
    map (name: import (containerDir + "/${name}") moduleArgs) (builtins.attrNames nixFiles);
in
  lib.mkMerge ([
      {
        systemd.tmpfiles.rules = [
          "d /var/lib/containers 0755 root root -"
        ];

        virtualisation.oci-containers.backend = "podman";

        systemd.timers = {
          updatecontainers = {
            timerConfig = {
              Unit = "updatecontainers.service";
              OnCalendar = "Mon 02:00";
            };
            wantedBy = ["timers.target"];
          };
        };

        systemd.services = let
          containerNames = builtins.attrNames config.virtualisation.oci-containers.containers;
        in
          {
            updatecontainers = {
              serviceConfig = {
                Type = "oneshot";
                ExecStart = "update-containers";
              };
            };
          }
          // builtins.listToAttrs (map (name: {
              name = "podman-${name}";
              value = {
                after = ["systemd-tmpfiles-setup.service"];
                wants = ["systemd-tmpfiles-setup.service"];
              };
            })
            containerNames);
      }
    ]
    ++ containerModules)
