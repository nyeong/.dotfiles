# Kavita - E-reader
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
in {
  systemd.tmpfiles.rules = [
    "d /var/lib/containers/kavita 0755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.kavita = {
    image = "linuxserver/kavita:latest";
    ports = ["5000:5000"];
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
