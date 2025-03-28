{ user }:
{
  # TODO: casks로 깔까 nix로 깔까...
  homebrew.casks = [ "hammerspoon" ];

  system.defaults.CustomUserPreferences = {
    "org.hammerspoon.Hammerspoon" = {
      MJConfigFile = "~/.config/hammerspoon/init.lua";
    };
  };

  home-manager.users.${user}.home = {
    file.".config/hammerspoon" = {
      source = ./config;
      recursive = true;
    };
  };
}
