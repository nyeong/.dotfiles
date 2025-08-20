{
  config,
  pkgs,
  userConfig,
  secrets,
  ...
}: {
  imports = [
    ./services/caddy.nix
    ./services/rclone.nix
    ./hardware-configuration.nix
    ./containers
    ../../modules/system/emacs
  ];

  # agenix secrets
  age.secrets.webdav-password = {
    file = "${secrets}/webdav-password.age";
    owner = "nyeong";
    group = "users";
    mode = "0400";
  };

  # agenix identity keys
  age.identityPaths = ["/etc/ssh/ssh_host_ed25519_key"];

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

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "nixbox";
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [22 22000 8384];
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
    startWhenNeeded = true;
  };

  # SSH Agent
  programs.ssh.startAgent = true;

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
    bottom
    helix

    rclone

    # Cursor SSH
    wget
    nodejs
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
