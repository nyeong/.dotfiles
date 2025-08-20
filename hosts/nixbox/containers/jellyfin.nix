# Jellyfin - Media server
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
in {
  systemd.tmpfiles.rules = [
    "d /var/lib/containers/jellyfin 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/jellyfin/config 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/jellyfin/cache 0755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.jellyfin = {
    image = "ghcr.io/jellyfin/jellyfin:latest";
    environment = {
      PUID = puid;
      PGID = pgid;
      TZ = tz;
    };
    volumes = [
      "/storage/@flims:/media"
      "/var/lib/containers/jellyfin/config:/config"
      "/var/lib/containers/jellyfin/cache:/cache"
    ];
    ports = [
      "8096:8096"
    ];
    extraOptions = [
      "--network=host"
    ];
    autoStart = true;
  };
}
