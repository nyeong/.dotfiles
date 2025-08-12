{
  config,
  pkgs,
  userConfig,
  ...
}: {
  # rclone WebDAV
  systemd.services.rclone-webdav = {
    description = "rclone WebDAV server for library";
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "simple";
      User = "nyeong";
      ExecStart = ''
        ${pkgs.rclone}/bin/rclone serve webdav /storage/@library \
        --addr :8080 \
        --user nyeong \
        --pass 'tNyoKUwUKXxQ9oVWh8j14lVzr8tbb4LY_8meve8'
      '';
      Restart = "on-failure";
    };
  };

  virtualisation.oci-containers.containers.calibre-web = {
    image = "linuxserver/calibre-web:latest";
    ports = ["8083:8083"];
    environment = {
      PUID = "1000";
      PGID = "100";
      TZ = "Asia/Seoul";
    };
    volumes = [
      "/storage/@library:/books"
    ];
  };

  nix = {
    settings = {
      experimental-features = ["nix-command" "flakes"];
      auto-optimise-store = true;
      trusted-users = ["root" "nyeong"];
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  imports = [
    ./hardware-configuration.nix
    ../../modules/system/emacs
  ];

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  # networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.
  networking = {
    hostName = "nixbox";
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [22 80 443 22000 8384];
      allowedUDPPorts = [22000 21027];
    };
  };

  time.timeZone = "Asia/Seoul";
  i18n.defaultLocale = "en_US.UTF-8";

  swapDevices = [
    {
      device = "/swapfile";
      size = 32 * 1024;
    }
  ];

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # Ensure system zsh is enabled when user's login shell is zsh
  programs.zsh.enable = true;

  users.users.nyeong = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager" "docker"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHW38hDRPSN1QuPELEBOj5ex6mV9Iw69z6jJRdveibGE me@annyeong.me"
    ];
    shell = pkgs.zsh;
  };

  virtualisation.podman = {
    enable = true;
    defaultNetwork.settings.dns_enabled = true;
  };

  environment.systemPackages = with pkgs; [
    vim
    git
    htop
    helix

    rclone
  ];

  services.btrfs.autoScrub = {
    enable = true;
    interval = "monthly";
    fileSystems = ["/storage"];
  };

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.05"; # Did you read the comment?
}
