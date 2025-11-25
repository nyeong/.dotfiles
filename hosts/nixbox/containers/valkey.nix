# alternative to redis
{
  containerConfig,
  palette,
  ...
}: let
  inherit (containerConfig) puid pgid tz;
  cfg = palette.nixbox.services;
in {
  systemd.tmpfiles.rules = [
    "Z /var/lib/containers/valkey 2755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.valkey = {
    image = "valkey/valkey";
    extraOptions = ["--network=host"];
    ports = ["${toString cfg.valkey.port}:6379"];
    volumes = [
      "/var/lib/containers/valkey:/data"
    ];
    autoStart = true;
  };
}
