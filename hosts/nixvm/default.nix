{ config, inputs, pkgs, userConfig, ... }: {
  imports = [
    ../../modules/fonts
    ../../modules/fonts/monoplex
    ../../modules/fonts/sf-mono
  ];

  users.users.${userConfig.username} = {
    name = "${userConfig.username}";
    home = "/home/${userConfig.username}";
    isNormalUser = true;
    extraGroups = [
      "wheel"
    ];
    isHidden = false;
    shell = pkgs.zsh;
  };

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 42;
      };
      efi.canTouchEfiVariables = true;
    };
    initrd.availableKernelModules =
      [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
    # Uncomment for AMD GPU
    # initrd.kernelModules = [ "amdgpu" ];
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "uinput" ];
  };

  # Set your time zone.
  time.timeZone = "Asia/Seoul";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    hostName = "%HOST%"; # Define your hostname.
    useDHCP = false;
    interfaces."%INTERFACE%".useDHCP = true;
  };

  nix = {
    nixPath =
      [ "nixos-config=/home/${userConfig.username}/.local/share/src/nixos-config:/etc/nixos" ];
    settings = {
      allowed-users = [ "${userConfig.username}" ];
      trusted-users = [ "@admin" "${userConfig.username}" ];
      substituters =
        [ "https://nix-community.cachix.org" "https://cache.nixos.org" ];
      trusted-public-keys =
        [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
    };

    package = pkgs.nix;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Manages keys and such
  programs = {
    gnupg.agent.enable = true;

    # Needed for anything GTK related
    dconf.enable = true;

    # My shell
    zsh.enable = true;
  };

  services = {
    xserver = {
      enable = true;

      # Uncomment these for AMD or Nvidia GPU
      # videoDrivers = [ "amdgpu" ];
      # videoDrivers = [ "nvidia" ];

      # Uncomment this for Nvidia GPU
      # This helps fix tearing of windows for Nvidia cards
      # services.xserver.screenSection = ''
      #   Option       "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
      #   Option       "AllowIndirectGLXProtocol" "off"
      #   Option       "TripleBuffer" "on"
      # '';

      # LightDM Display Manager
      displayManager.defaultSession = "none+bspwm";
      displayManager.lightdm = {
        enable = true;
        greeters.slick.enable = true;
        background = ../../modules/nixos/config/login-wallpaper.png;
      };

      # Tiling window manager
      windowManager.bspwm = { enable = true; };

      # Turn Caps Lock into Ctrl
      layout = "us";
      xkbOptions = "ctrl:nocaps";

      # Better support for general peripherals
      libinput.enable = true;
    };

    # Let's be able to SSH into this machine
    openssh.enable = true;


    gvfs.enable = true; # Mount, trash, and other functionalities
    tumbler.enable = true; # Thumbnail support for images

    # Emacs runs as a daemon
    emacs = {
      enable = true;
      package = pkgs.emacs-unstable;
    };
  };

  # When emacs builds from no cache, it exceeds the 90s timeout default
  systemd.user.services.emacs = { serviceConfig.TimeoutStartSec = "7min"; };

  # Enable CUPS to print documents
  # services.printing.enable = true;
  # services.printing.drivers = [ pkgs.brlaser ]; # Brother printer driver

  # Enable sound
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Video support
  hardware = {
    opengl.enable = true;
    # nvidia.modesetting.enable = true;

    # Enable Xbox support
    # xone.enable = true;

    # Crypto wallet support
    ledger.enable = true;
  };



  # Don't require password for users in `wheel` group for these commands
  security.sudo = {
    enable = true;
    extraRules = [{
      commands = [{
        command = "${pkgs.systemd}/bin/reboot";
        options = [ "NOPASSWD" ];
      }];
      groups = [ "wheel" ];
    }];
  };

  fonts.packages = with pkgs; [
    dejavu_fonts
    emacs-all-the-icons-fonts
    feather-font # from overlay
    jetbrains-mono
    font-awesome
    noto-fonts
    noto-fonts-emoji
  ];

  system.stateVersion = "21.05"; # Don't change this
}
