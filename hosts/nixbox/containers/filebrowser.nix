# File Browser - File manager
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
  palette = import ../_palette.nix;
in {
  # Create container directories with proper permissions
  systemd.tmpfiles.rules = [
    "d /var/lib/containers/filebrowser 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/filebrowser/config 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/filebrowser/database 0755 ${puid} ${pgid} -"
  ];

  # File Browser - File manager
  virtualisation.oci-containers.containers.filebrowser = {
    image = "filebrowser/filebrowser:latest";
    ports = ["${palette.ports.filebrowser}:8081"];
    environment = {
      PUID = puid;
      PGID = pgid;
      TZ = tz;
    };
    extraOptions = [
      "--user=${puid}:${pgid}"
    ];
    cmd = [
      "--port"
      "${palette.ports.filebrowser}"
    ];
    volumes = [
      "/storage:/srv"
      "/var/lib/containers/filebrowser/config:/config"
      "/var/lib/containers/filebrowser/database:/database"
    ];
    autoStart = true;
  };
}
