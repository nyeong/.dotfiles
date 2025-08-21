# For serving multiple directories as WebDAV server
{
  config,
  secrets,
  pkgs,
  ...
}: let
  palette = import ../../_palette.nix;
in {
  systemd.services.rclone-storage = {
    description = "rclone WebDAV server for storage";
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "simple";
      User = "nyeong";
      WorkingDirectory = "/storage";
      ExecStart = ''
        ${pkgs.rclone}/bin/rclone serve webdav storage: \
        --addr :${palette.ports.webdav_storage} \
        --user nyeong \
        --config ${./config/rclone.conf} \
        --htpasswd ${config.age.secrets."rclone.htpasswd".path}
      '';
      Restart = "on-failure";
    };
  };

  systemd.services.rclone-hanassig = {
    description = "rclone WebDAV server for hanassig";
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "simple";
      User = "nyeong";
      WorkingDirectory = "/home/nyeong/hanassig";
      ExecStart = ''
        ${pkgs.rclone}/bin/rclone serve webdav hanassig: \
        --addr :${palette.ports.webdav_hanassig} \
        --user nyeong \
        --config ${./config/rclone.conf} \
        --htpasswd ${config.age.secrets."rclone.htpasswd".path}
      '';
      Restart = "on-failure";
    };
  };
}
