{
  palette,
  pkgs,
  ...
}: {
  imports = [
    ../../home
  ];

  home.username = palette.user.username;
  home.homeDirectory = "/home/${palette.user.username}";
  home.stateVersion = "25.11";

  features.devTools.enable = true;

  features.syncthing = {
    enable = true;
    folders = {
      "screenshots@nyeong-air" = "/storage/@screenshots/nyeong-air";
      "hanassig" = "~/hanassig";
      "dotfiles" = "~/.dotfiles";
    };
  };
}
