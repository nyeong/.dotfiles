{userConfig, ...}: {
  homebrew.casks = ["karabiner-elements"];

  home-manager.users.${userConfig.username}.home.file = {
    ".config/karabiner/assets/complex_modifications/nyeong.json" = {
      source = ./config/nyeong.json;
    };
  };
}
