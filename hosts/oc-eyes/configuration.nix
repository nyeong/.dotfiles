{
  palette,
  pkgs,
  ...
}: {
  imports = [
    ../../modules
    ./services
  ];

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
