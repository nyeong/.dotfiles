# alternative to redis
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
  palette = import ../_palette.nix;
in {
  systemd.tmpfiles.rules = [
    "Z /var/lib/containers/valkey 2755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.valkey = {
    image = "valkey/valkey";
    extraOptions = ["--network=host"];
    ports = ["${palette.ports.valkey}:6379"];
    volumes = [
      "/var/lib/containers/valkey:/data"
    ];
    autoStart = true;
  };
}
