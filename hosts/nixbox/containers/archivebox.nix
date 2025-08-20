# ArchiveBox - Web archiving tool
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
in {
  systemd.tmpfiles.rules = [
    "d /storage/@archives/archivebox 0755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.archivebox = {
    image = "ghcr.io/archivebox/archivebox:latest";
    ports = ["8001:8000"];
    environment = {
      PUID = puid;
      PGID = pgid;
      TZ = tz;
    };
    volumes = [
      "/storage/@archives/archivebox:/data"
    ];
    extraOptions = [
      "--user=${puid}:${pgid}"
    ];
    autoStart = true;
  };
}
