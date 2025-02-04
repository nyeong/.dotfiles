{ self, userConfig, pkgs, inputs, ... }: {

  imports = [
    ../modules/shared
  ];

  nix.settings.experimental-features = "nix-command flakes";
  nix.useDaemon = true;

  services.nix-daemon.enable = true;

  environment.systemPackages = with pkgs; [
    emacs-unstable
  ] ++ (import ../modules/shared/packages.nix { inherit pkgs; });

  programs.zsh.enable = true;

  system.configurationRevision = self.rev or self.dirtyRev or null;
  system.stateVersion = 5;

  security.pam.enableSudoTouchIdAuth = true;

  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;

  users.users.${userConfig.username} = {
    home = userConfig.home;
  };

  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
    nerd-fonts.fira-code
  ];
}
