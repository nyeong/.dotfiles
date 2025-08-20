# alternative to redis
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
in {
  systemd.tmpfiles.rules = [
    "d /var/lib/containers/valkey 0755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.valkey = {
    image = "valkey/valkey";
    extraOptions = ["--network=host"];
    ports = ["6379:6379"];
    volumes = [
      "/var/lib/containers/valkey:/data"
    ];
    autoStart = true;
  };
}
