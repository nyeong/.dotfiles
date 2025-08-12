{
  config,
  pkgs,
  userConfig,
  ...
}: {
  # Enable OCI containers with Podman backend
  virtualisation.oci-containers.backend = "podman";

  # Kavita - Comic/Manga reader
  virtualisation.oci-containers.containers.kavita = {
    image = "linuxserver/kavita:latest";
    ports = ["5000:5000"];
    environment = {
      PUID = "1000";
      PGID = "100";
      TZ = "Asia/Seoul";
    };
    volumes = [
      "/storage/@library:/manga"
      "/var/lib/containers/kavita:/config"
    ];
    autoStart = true;
    extraOptions = ["--restart=unless-stopped"];
  };

  # Calibre Web - eBook library manager
  virtualisation.oci-containers.containers.calibre-web = {
    image = "linuxserver/calibre-web:latest";
    ports = ["8083:8083"];
    environment = {
      PUID = "1000";
      PGID = "100";
      TZ = "Asia/Seoul";
    };
    volumes = [
      "/storage/@library:/books"
      "/var/lib/containers/calibre-web:/config"
    ];
    autoStart = true;
    extraOptions = ["--restart=unless-stopped"];
  };
}
