{ user }: {
  homebrew.casks = [ "karabiner-elements" ];

  home-manager.users.${user}.home.file = {
    ".config/karabiner/assets/complex_modifications/nyeong.json" = {
      source = ./config/nyeong.json;
    };
  };
}
