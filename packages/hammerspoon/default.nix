{ user }:
{
  homebrew.casks = [ "hammerspoon" ];

  home-manager.users.${user}.home =  {
    file.".config/hammerspoon" = {
      source = ./config;
      recursive = true;
    };
  };
}