{
  userConfig,
  pkgs,
  ...
}: {
  imports = [
    ../../modules/home/zsh
    ../../modules/home/ssh.nix
    ../../modules/home/git.nix
    ../../modules/home/syncthing
  ];

  home.username = userConfig.username;
  home.homeDirectory = "/home/${userConfig.username}";
  home.stateVersion = "25.11";

  home.packages = with pkgs; [
    git
    lsd
    coreutils
    ripgrep
    fd
  ];

  # nixbox에서 syncthing 폴더 경로 설정
  services.syncthing.settings.folders = {
    "screenshots@nyeong-air".path = "/storage/@screenshots/nyeong-air";
    "hanassig".path = "~/hanassig";
    "dotfiles".path = "~/.dotfiles";
  };
}
