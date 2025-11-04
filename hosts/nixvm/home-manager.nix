{palette, ...}: {
  imports = [
    ../../home
  ];

  home.username = palette.user.username;
  home.homeDirectory = "/home/${palette.user.username}";
  home.stateVersion = "25.11";
}
