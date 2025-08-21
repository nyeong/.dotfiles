# Immich - Photo management and backup
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
  palette = import ../_palette.nix;
in {
  systemd.tmpfiles.rules = [
    "d /var/lib/containers/immich 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/immich/config 0755 ${puid} ${pgid} -"
    "d /storage/@immich/photos 0755 ${puid} ${pgid} -"
  ];

  # Immich Server - Photo management and backup
  virtualisation.oci-containers.containers.immich-server = {
    image = "ghcr.io/immich-app/immich-server:release";
    extraOptions = ["--network=host"];
    ports = ["${palette.ports.immich}:8000"];
    environment = {
      # Database
      DB_HOSTNAME = "localhost";
      DB_PORT = "${palette.ports.postgres}";
      DB_USERNAME = "immich";
      DB_PASSWORD = "immich123";
      DB_DATABASE_NAME = "immich";

      # Redis
      REDIS_HOSTNAME = "localhost";
      REDIS_PORT = "${palette.ports.valkey}";

      IMMICH_MACHINE_LEARNING_ENABLED = "true";

      # Timezone
      TZ = tz;
    };
    volumes = [
      "/storage/@immich/photos:/data"
      "/etc/localtime:/etc/localtime:ro"
    ];
    dependsOn = ["postgres" "valkey"];
    autoStart = true;
  };
}
