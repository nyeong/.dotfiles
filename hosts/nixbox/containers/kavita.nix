# Kavita - E-reader
{
  containerConfig,
  palette,
  ...
}: let
  inherit (containerConfig) puid pgid tz;
  nixbox = palette.nixbox;
in {
  systemd.tmpfiles.rules = [
    "Z /var/lib/containers/kavita 2755 ${puid} ${pgid} -"
  ];

  virtualisation.oci-containers.containers.kavita = {
    image = "linuxserver/kavita:latest";
    ports = ["${nixbox.network.ports.kavita}:5000"];
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
