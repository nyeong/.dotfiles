{pkgs, ...}: {
  home = {
    packages = [pkgs.zellij];
    file.".config/zellij/config.kdl" = {source = ./config/config.kdl;};
    file.".config/zellij/layouts/remote-servers.kdl" = {source = ./config/layouts/remote-servers.kdl;};
  };
}
