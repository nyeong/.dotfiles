{
  config,
  pkgs,
  modulesPath,
  palette,
  lib,
  ...
}: {
  imports =
    [
      # Include the default lxd configuration.
      "${modulesPath}/virtualisation/lxc-container.nix"
      ../../modules
    ]
    # Conditionally import container-specific configurations if they exist
    # (only available when running inside the container)
    ++ lib.optionals (builtins.pathExists /etc/nixos/orbstack.nix) [
      /etc/nixos/orbstack.nix
    ]
    ++ lib.optionals (builtins.pathExists /etc/nixos/incus.nix) [
      /etc/nixos/incus.nix
    ];

  # Override user.nix for container-specific settings
  users.users.${palette.user.username} = {
    uid = 501;
    extraGroups = [
      "wheel"
      "orbstack"
    ];

    # simulate isNormalUser, but with an arbitrary UID
    isNormalUser = false;
    isSystemUser = true;
    group = "users";
    createHome = true;
    home = "/home/${palette.user.username}";
    homeMode = "700";
    useDefaultShell = true;
  };

  security.sudo.wheelNeedsPassword = false;
  users.mutableUsers = false;

  networking = {
    dhcpcd.enable = false;
    useDHCP = false;
    useHostResolvConf = false;
  };

  documentation.enable = lib.mkForce false;

  systemd.network = {
    enable = true;
    networks."50-eth0" = {
      matchConfig.Name = "eth0";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = true;
      };
      linkConfig.RequiredForOnline = "routable";
    };
  };

  system.stateVersion = "25.05";
}
