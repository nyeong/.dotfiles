{
  isDarwin,
  isLinux,
  palette,
  ...
}: {
  sops.age.keyFile =
    if isDarwin
    then "/Users/${palette.user.username}/Library/Application Support/sops/age/keys.txt"
    else "/home/${palette.user.username}/.config/sops/age/keys.txt";
}
