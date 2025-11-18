{
  palette,
  pkgs,
  ...
}: {
  imports = [
    ../../modules
  ];

  users.users.${palette.user.username} = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHW38hDRPSN1QuPELEBOj5ex6mV9Iw69z6jJRdveibGE me@annyeong.me"
    ];
    shell = pkgs.zsh;
  };
  programs.zsh.enable = true;

  zramSwap.enable = true;
  networking.hostName = "oc-eyes";
  networking.domain = "subnet11141831.oc.oraclevcn.com";
  services.openssh.enable = true;
  services.tailscale.enable = true;

  system.stateVersion = "25.11";

  boot.loader.grub.configurationLimit = 2;
  boot.tmp.cleanOnBoot = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
