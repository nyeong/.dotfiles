# ArchiveBox - Web archiving tool
{
  containerConfig,
  palette,
  ...
}: let
  inherit (containerConfig) puid pgid tz;
  cfg = palette.nixbox.services;
in {
  systemd.tmpfiles.rules = [
    "Z /storage/@archives/archivebox 2755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.archivebox = {
    image = "ghcr.io/archivebox/archivebox:latest";
    ports = ["${toString cfg.archivebox.port}:8000"];
    environment = {
      CSRF_TRUSTED_ORIGINS = "http://localhost:${toString cfg.archivebox.port}";
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
