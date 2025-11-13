{
  palette,
  config,
  ...
}: {
  imports = [
    ../../home
  ];

  home.username = palette.user.username;
  home.homeDirectory = "/Users/${palette.user.username}";
  home.stateVersion = "25.11";

  features.devTools.enable = true;

  features.syncthing = {
    enable = true;
    folders = {
      "screenshots@nyeong-air" = "~/Screenshots";
      "hanassig" = "~/hanassig";
      "dotfiles" = "~/.dotfiles";
    };
  };
}
