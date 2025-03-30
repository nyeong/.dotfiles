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
    file.".config/hammerspoon/Spoons/PaperWM.spoon" = {
      source = builtins.fetchGit {
        url = "https://github.com/mogenson/PaperWM.spoon.git";
        rev = "41389206e739e6f48ea59ddcfc07254226f4c93f";
      };
      recursive = true;
    };
  };
}
