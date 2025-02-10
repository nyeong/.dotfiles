{ user }:
{
  # TODO: casks로 깔까 nix로 깔까...
  homebrew.casks = [ "hammerspoon" ];

  home-manager.users.${user}.home =  {
    file.".config/hammerspoon" = {
      source = ./config;
      recursive = true;
    };
  };
}