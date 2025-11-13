# alternative to redis
{
  containerConfig,
  palette,
  ...
}: let
  inherit (containerConfig) puid pgid tz;
  nixbox = palette.nixbox;
in {
  systemd.tmpfiles.rules = [
    "Z /var/lib/containers/valkey 2755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.valkey = {
    image = "valkey/valkey";
    extraOptions = ["--network=host"];
    ports = ["${nixbox.network.ports.valkey}:6379"];
    volumes = [
      "/var/lib/containers/valkey:/data"
    ];
    autoStart = true;
  };
}
