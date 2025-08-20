{
  config,
  pkgs,
  ...
}: {
  systemd.services.rclone-webdav = {
    description = "rclone WebDAV server for library";
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "simple";
      User = "nyeong";
      ExecStart = ''
        ${pkgs.rclone}/bin/rclone serve webdav /storage \
        --addr :8080 \
        --user nyeong \
        --pass $(cat ${config.age.secrets.webdav-password.path})
      '';
      Restart = "on-failure";
    };
  };
}
