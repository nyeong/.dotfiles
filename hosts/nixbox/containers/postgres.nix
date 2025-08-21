{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
  palette = import ../_palette.nix;
in {
  systemd.tmpfiles.rules = [
    "d /var/lib/containers/postgres 0755 ${puid} ${pgid} -"
  ];

  # https://immich.app/docs/administration/postgres-standalone/
  virtualisation.oci-containers.containers.postgres = {
    image = "postgres:16";
    extraOptions = ["--network=host"];
    ports = ["${palette.ports.postgres}:5432"];
    environment = {
      POSTGRES_USER = "immich";
      POSTGRES_PASSWORD = "immich123";
      POSTGRES_DB = "immich";
      POSTGRES_INITDB_ARGS = "--data-checksums";
    };
    volumes = [
      "/var/lib/containers/postgres:/var/lib/postgresql/data"
    ];
    autoStart = true;
  };
}
