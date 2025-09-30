# Calibre Web - eBook library manager
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
  palette = import ../_palette.nix;
in {
  systemd.tmpfiles.rules = [
    "Z /var/lib/containers/calibre-web 2755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.calibre-web = {
    image = "linuxserver/calibre-web:latest";
    ports = ["${palette.ports.calibre-web}:8083"];
    environment = {
      PUID = puid;
      PGID = pgid;
      TZ = tz;
    };
    volumes = [
      "/storage/@library:/books"
      "/var/lib/containers/calibre-web:/config"
    ];
    autoStart = true;
  };
}
