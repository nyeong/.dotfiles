{
  pkgs,
  palette,
  inputs,
  overlays,
  ...
}: {
  imports = [
    inputs.nix-homebrew.darwinModules.nix-homebrew
    ../../modules
  ];

  users.users.${palette.user.username} = {
    name = "${palette.user.username}";
    home = "/Users/${palette.user.username}";
    isHidden = false;
    shell = pkgs.zsh;
  };

  # Mac App Store 앱
  # $ nix run nixpkgs#mas -- search <app name>
  # If you have previously added these apps to your Mac App Store profile (but not installed them on this system),
  # you may receive an error message "Redownload Unavailable with This Apple ID".
  # This message is safe to ignore. (https://github.com/dustinlyons/nixos-config/issues/83)
  homebrew.masApps = {
    "KakaoTalk" = 869223134;
  };

  ids.gids.nixbld = 350;

  networking = {
    hostName = "nyeong-air";
    localHostName = "nyeong-air";
    computerName = "Nyeong's Air";
  };
}
