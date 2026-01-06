# Home profile for nixbox
{...}: {
  imports = [
    ../../modules/home
  ];

  features.syncthing = {
    enable = true;
    folders = {
      "screenshots@nyeong-air" = "/storage/@screenshots/nyeong-air";
      "hanassig" = "/srv/hanassig";
      "dotfiles" = "/srv/dotfiles";
    };
  };
}
