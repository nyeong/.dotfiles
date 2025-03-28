# macOS 패키지 관리
{ pkgs, ... }:
let
  user = "nyeong";
  # 별도의 설정이 필요한 프로그램
  packages = [
    # 자동화 도구
    (import ./karabiner { inherit user; })
    (import ./hammerspoon { inherit user; })
  ];

  # 단순 cask로 설치할 프로그램
  casks = [
    # Communication Tools
    "discord"
    "notion"
    "slack"

    # Utility Tools
    "syncthing"

    # Productivity Tools
    "raycast"

    # Browsers
    "arc"

    "kicad"
    "datagrip"
    "linearmouse"
    "medis"
    "tailscale"

    "cursor"

    "nextcloud"
  ];

  # home-manager로 설치할 프로그램
  home-packages = with pkgs; [
    dockutil
    pinentry_mac

    raycast

    # Archive
    keka
    # the-unarchiver
  ];

  # Mac App Store 앱
  # $ nix run nixpkgs#mas -- search <app name>
  # If you have previously added these apps to your Mac App Store profile (but not installed them on this system),
  # you may receive an error message "Redownload Unavailable with This Apple ID".
  # This message is safe to ignore. (https://github.com/dustinlyons/nixos-config/issues/83)
  masApps = {
    # "wireguard" = 1451685025;
    "KakaoTalk" = 869223134;
  };
in
{
  imports = packages ++ [ ./share.nix ];

  homebrew = {
    enable = true;
    casks = casks;
    masApps = masApps;
  };

  home-manager.users.${user}.home = {
    packages = home-packages;
  };
}
