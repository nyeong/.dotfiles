{
  config,
  isDarwin,
  isLinux,
  lib,
  ...
}: {
  sops.age.keyFile =
    if isDarwin
    then lib.mkDefault "${config.home.homeDirectory}/Library/Application Support/sops/age/keys.txt"
    else lib.mkDefault "${config.home.homeDirectory}/.config/sops/age/keys.txt";
}
