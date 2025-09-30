# Kavita - E-reader
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
  palette = import ../_palette.nix;
in {
  systemd.tmpfiles.rules = [
    "Z /var/lib/containers/kavita 2755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.kavita = {
    image = "linuxserver/kavita:latest";
    ports = ["${palette.ports.kavita}:5000"];
    environment = {
      PUID = puid;
      PGID = pgid;
      TZ = tz;
    };
    volumes = [
      "/storage/@library:/library"
      "/var/lib/containers/kavita:/config"
    ];
    autoStart = true;
  };
}
