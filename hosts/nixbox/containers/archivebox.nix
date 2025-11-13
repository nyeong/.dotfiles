# ArchiveBox - Web archiving tool
{
  containerConfig,
  palette,
  ...
}: let
  inherit (containerConfig) puid pgid tz;
  nixbox = palette.nixbox;
in {
  systemd.tmpfiles.rules = [
    "Z /storage/@archives/archivebox 2755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.archivebox = {
    image = "ghcr.io/archivebox/archivebox:latest";
    ports = ["${nixbox.network.ports.archivebox}:8000"];
    environment = {
      CSRF_TRUSTED_ORIGINS = "http://localhost:8001";
      PUID = puid;
      PGID = pgid;
      TZ = tz;
    };
    volumes = [
      "/storage/@archives/archivebox:/data"
    ];
    autoStart = true;
  };
}
