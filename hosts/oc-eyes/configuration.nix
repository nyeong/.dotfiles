{
  palette,
  pkgs,
  ...
}: {
  imports = [
    ../../modules/system
    ./services
  ];

  # Secrets management
  sops.defaultSopsFile = ../../secrets/api-keys.yaml;

  zramSwap.enable = true;

  networking.hostName = "oc-eyes";
  networking.domain = "subnet11141831.oc.oraclevcn.com";
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.tailscale.enable = true;
  services.tailscale.permitCertUid = "caddy";

  system.stateVersion = "25.11";

  boot.loader.grub.configurationLimit = 2;
  boot.tmp.cleanOnBoot = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
