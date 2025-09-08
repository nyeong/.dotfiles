# Immich - Photo management and backup
# Immich는 전용으로 Postgres를 돌리길 권장함
{
  containerConfig,
  config,
  pkgs,
  ...
}: let
  inherit (containerConfig) puid pgid tz;
  palette = import ../_palette.nix;
in {
  systemd.services.create-immich-network = {
    serviceConfig.Type = "oneshot";
    wantedBy = ["multi-user.target"];
    script = ''
      ${pkgs.podman}/bin/podman network ls | grep -q immich-net || \
      ${pkgs.podman}/bin/podman network create immich-net
    '';
  };

  systemd.tmpfiles.rules = [
    # SSD
    "d /var/lib/containers/immich 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/immich/config 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/immich/postgres 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/immich/ml 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/immich/valkey 0755 999 999 -"

    # HDD
    "d /storage/@immich 0755 ${puid} ${pgid} -"
    "d /storage/@immich/photos 0755 ${puid} ${pgid} -"
  ];

  # Immich Server - Photo management and backup
  virtualisation.oci-containers.containers.immich-server = {
    image = "ghcr.io/immich-app/immich-server:release";
    extraOptions = [
      "--network=immich-net"
    ];
    ports = ["${palette.ports.immich}:2283"];
    environment = {
      # Database
      DB_HOSTNAME = "immich-postgres";
      DB_PORT = "5432";
      DB_USERNAME = "immich";
      DB_PASSWORD = "immich123";
      DB_DATABASE_NAME = "immich";

      # Redis
      REDIS_HOSTNAME = "immich-valkey";
      REDIS_PORT = "6379";
      REDIS_DB_INDEX = "1";

      IMMICH_MACHINE_LEARNING_ENABLED = "true";
      IMMICH_MACHINE_LEARNING_URL = "http://immich-ml:3003";

      # Timezone
      TZ = tz;
    };
    volumes = [
      "/storage/@immich/photos:/data"
      "/etc/localtime:/etc/localtime:ro"
    ];
    dependsOn = ["immich-postgres" "immich-ml" "immich-valkey"];
    autoStart = true;
  };

  virtualisation.oci-containers.containers.immich-ml = {
    image = "ghcr.io/immich-app/immich-machine-learning:release";
    extraOptions = ["--network=immich-net"];
    environment = {
      TZ = tz;
    };
    volumes = [
      "/var/lib/containers/immich/ml:/cache"
    ];
    autoStart = true;
  };

  virtualisation.oci-containers.containers.immich-postgres = {
    image = "ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0@sha256:32324a2f41df5de9efe1af166b7008c3f55646f8d0e00d9550c16c9822366b4a";
    extraOptions = ["--network=immich-net"];
    environment = {
      POSTGRES_PASSWORD = "immich123";
      POSTGRES_USER = "immich";
      POSTGRES_DB = "immich";
      POSTGRES_INITDB_ARGS = "--data-checksums";
      TZ = tz;
    };
    volumes = [
      "/var/lib/containers/immich/postgres:/var/lib/postgresql/data"
    ];
    autoStart = true;
  };

  virtualisation.oci-containers.containers.immich-valkey = {
    image = "docker.io/valkey/valkey:8-bookworm@sha256:a137a2b60aca1a75130022d6bb96af423fefae4eb55faf395732db3544803280";
    extraOptions = [
      "--network=immich-net"
      "--user=999:999"
    ];
    environment = {
      TZ = tz;
    };
    volumes = [
      "/var/lib/containers/immich/valkey:/data"
    ];
    autoStart = true;
  };
}
