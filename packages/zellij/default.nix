{ user, pkgs }:
{
  # TODO: 매번 home-manager.users... 이렇게 깊이 depth 들어가지 않고 쓸 방법은 없을까?
  home-manager.users.${user}.home =  {
    packages = [pkgs.zellij];
    file.".config/zellij/config.kdl" = {
      source = ./config/config.kdl;
    };
  };
}