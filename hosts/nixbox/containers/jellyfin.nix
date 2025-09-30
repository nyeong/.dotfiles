# Jellyfin - Media server
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
  palette = import ../_palette.nix;
in {
  systemd.tmpfiles.rules = [
    "Z /var/lib/containers/jellyfin 2755 ${puid} ${pgid} -"
    "Z /var/lib/containers/jellyfin/config 2755 ${puid} ${pgid} -"
    "Z /var/lib/containers/jellyfin/cache 2755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.jellyfin = {
    image = "ghcr.io/jellyfin/jellyfin:latest";
    environment = {
      PUID = puid;
      PGID = pgid;
      TZ = tz;
    };
    volumes = [
      "/storage/@films:/media"
      "/var/lib/containers/jellyfin/config:/config"
      "/var/lib/containers/jellyfin/cache:/cache"
    ];
    ports = [
      "${palette.ports.jellyfin}:8096"
    ];
    autoStart = true;
  };
}
