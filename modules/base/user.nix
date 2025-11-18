{
  palette,
  pkgs,
  lib,
  isLinux,
  isDarwin,
  ...
}: {
  users.users.${palette.user.username} = lib.mkMerge [
    {
      openssh.authorizedKeys.keys = lib.mkDefault [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHW38hDRPSN1QuPELEBOj5ex6mV9Iw69z6jJRdveibGE me@annyeong.me"
      ];
      shell = pkgs.zsh;
    }
    (lib.mkIf isLinux {
      isNormalUser = lib.mkDefault true;
      extraGroups = lib.mkDefault ["wheel"];
      home = lib.mkDefault "/home/${palette.user.username}";
    })
    (lib.mkIf isDarwin {
      isHidden = lib.mkDefault false;
      name = lib.mkDefault palette.user.username;
      home = lib.mkDefault "/Users/${palette.user.username}";
    })
  ];
  programs.zsh.enable = true;
  programs.gnupg.agent.enable = true;
}
