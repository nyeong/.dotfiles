{
  userConfig,
  pkgs,
  ...
}: {
  imports = [
    ../../modules/home/zsh
    ../../modules/home/ssh.nix
    ../../modules/home/git.nix
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
}
