# Home profile for nyeong-air
{...}: {
  imports = [
    ../../modules/home
  ];

  features.devTools.enable = true;
  features.syncthing = {
    enable = true;
    folders = {
      "screenshots@nyeong-air" = "~/Screenshots";
      "hanassig" = "~/hanassig";
      "dotfiles" = "~/.dotfiles";
    };
  };
  features.hledger.enable = true;
}
