{pkgs, ...}: {
  home = {
    packages = [pkgs.zellij];
    file.".config/zellij/config.kdl" = {source = ./config/config.kdl;};
  };
}
