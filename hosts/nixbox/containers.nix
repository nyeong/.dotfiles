{
  config,
  pkgs,
  userConfig,
  ...
}: let
  puid = "1000";
  pgid = "100";
in {
  # Create container directories with proper permissions
  systemd.tmpfiles.rules = [
    "d /var/lib/containers 0755 root root -"
    "d /var/lib/containers/kavita 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/calibre-web 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/filebrowser 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/filebrowser/config 0755 ${puid} ${pgid} -"
    "d /var/lib/containers/filebrowser/database 0755 ${puid} ${pgid} -"
  ];

  # Enable OCI containers with Podman backend
  virtualisation.oci-containers.backend = "podman";

  # Kavita - Comic/Manga reader
  virtualisation.oci-containers.containers.kavita = {
    image = "linuxserver/kavita:latest";
    ports = ["5000:5000"];
    environment = {
      PUID = puid;
      PGID = pgid;
      TZ = "Asia/Seoul";
    };
    volumes = [
      "/storage/@library:/manga"
      "/var/lib/containers/kavita:/config"
    ];
    autoStart = true;
  };

  # Calibre Web - eBook library manager
  virtualisation.oci-containers.containers.calibre-web = {
    image = "linuxserver/calibre-web:latest";
    ports = ["8083:8083"];
    environment = {
      PUID = puid;
      PGID = pgid;
      TZ = "Asia/Seoul";
    };
    volumes = [
      "/storage/@library:/books"
      "/var/lib/containers/calibre-web:/config"
    ];
    autoStart = true;
  };

  # File Browser - File manager
  virtualisation.oci-containers.containers.filebrowser = {
    image = "filebrowser/filebrowser:latest";
    ports = ["8081:8081"];
    environment = {
      PUID = puid;
      PGID = pgid;
      TZ = "Asia/Seoul";
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

  # Ensure all podman containers start after tmpfiles has created volumes
  systemd.services = let
    containerNames = builtins.attrNames config.virtualisation.oci-containers.containers;
  in
    builtins.listToAttrs (map (name: {
        name = "podman-${name}";
        value = {
          after = ["systemd-tmpfiles-setup.service"];
          wants = ["systemd-tmpfiles-setup.service"];
        };
      })
      containerNames);
}
