{
  config,
  isDarwin,
  isLinux,
  ...
}: {
  sops.age.keyFile =
    if isDarwin
    then "${config.home.homeDirectory}/Library/Application Support/sops/age/keys.txt"
    else "${config.home.homeDirectory}/.config/sops/age/keys.txt";
}
