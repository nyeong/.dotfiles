{
  palette,
  isDarwin,
  isLinux,
  ...
}: {
  config = {
    home.username = "nyeong";
    home.homeDirectory =
      if isDarwin
      then "/Users/${palette.user.username}"
      else "/home/${palette.user.username}";
    home.stateVersion = "25.11";
  };
}
