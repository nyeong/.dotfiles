# File Browser - File manager
{containerConfig, ...}: let
  inherit (containerConfig) puid pgid tz;
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
    ports = ["8081:8081"];
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
      "8081"
    ];
    volumes = [
      "/storage:/srv"
      "/var/lib/containers/filebrowser/config:/config"
      "/var/lib/containers/filebrowser/database:/database"
    ];
    autoStart = true;
  };
}
